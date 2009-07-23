/* $Id$ */

/** @file waypoint_cmd.cpp Command Handling for waypoints. */

#include "stdafx.h"

#include "command_func.h"
#include "landscape.h"
#include "economy_func.h"
#include "bridge_map.h"
#include "town.h"
#include "waypoint_base.h"
#include "yapf/yapf.h"
#include "strings_func.h"
#include "gfx_func.h"
#include "functions.h"
#include "window_func.h"
#include "date_func.h"
#include "vehicle_func.h"
#include "string_func.h"
#include "company_func.h"
#include "newgrf_station.h"
#include "viewport_func.h"
#include "train.h"
#include "water.h"

#include "table/strings.h"

/**
 * Update the virtual coords needed to draw the waypoint sign.
 */
void Waypoint::UpdateVirtCoord()
{
	Point pt = RemapCoords2(TileX(this->xy) * TILE_SIZE, TileY(this->xy) * TILE_SIZE);
	SetDParam(0, this->index);
	this->sign.UpdatePosition(pt.x, pt.y - 0x20, STR_WAYPOINT_VIEWPORT);
}

/**
 * Set the default name for a waypoint
 * @param wp Waypoint to work on
 */
void MakeDefaultWaypointName(Waypoint *wp)
{
	uint32 used = 0; // bitmap of used waypoint numbers, sliding window with 'next' as base
	uint32 next = 0; // first waypoint number in the bitmap
	WaypointID idx = 0; // index where we will stop

	wp->town = ClosestTownFromTile(wp->xy, UINT_MAX);

	/* Find first unused waypoint number belonging to this town. This can never fail,
	 * as long as there can be at most 65535 waypoints in total.
	 *
	 * This does 'n * m' search, but with 32bit 'used' bitmap, it needs at most 'n * (1 + ceil(m / 32))'
	 * steps (n - number of waypoints in pool, m - number of waypoints near this town).
	 * Usually, it needs only 'n' steps.
	 *
	 * If it wasn't using 'used' and 'idx', it would just search for increasing 'next',
	 * but this way it is faster */

	WaypointID cid = 0; // current index, goes to Waypoint::GetPoolSize()-1, then wraps to 0
	do {
		Waypoint *lwp = Waypoint::GetIfValid(cid);

		/* check only valid waypoints... */
		if (lwp != NULL && wp != lwp) {
			/* only waypoints with 'generic' name within the same city */
			if (lwp->name == NULL && lwp->town == wp->town && lwp->string_id == wp->string_id) {
				/* if lwp->town_cn < next, uint will overflow to '+inf' */
				uint i = (uint)lwp->town_cn - next;

				if (i < 32) {
					SetBit(used, i); // update bitmap
					if (i == 0) {
						/* shift bitmap while the lowest bit is '1';
						 * increase the base of the bitmap too */
						do {
							used >>= 1;
							next++;
						} while (HasBit(used, 0));
						/* when we are at 'idx' again at end of the loop and
						 * 'next' hasn't changed, then no waypoint had town_cn == next,
						 * so we can safely use it */
						idx = cid;
					}
				}
			}
		}

		cid++;
		if (cid == Waypoint::GetPoolSize()) cid = 0; // wrap to zero...
	} while (cid != idx);

	wp->town_cn = (uint16)next; // set index...
	wp->name = NULL; // ... and use generic name
}

/**
 * Find a deleted waypoint close to a tile.
 * @param tile to search from
 * @param str  the string to get the 'type' of
 * @return the deleted nearby waypoint
 */
static Waypoint *FindDeletedWaypointCloseTo(TileIndex tile, StringID str)
{
	Waypoint *wp, *best = NULL;
	uint thres = 8;

	FOR_ALL_WAYPOINTS(wp) {
		if ((wp->facilities & ~FACIL_WAYPOINT) == 0 && wp->string_id == str && (wp->owner == _current_company || wp->owner == OWNER_NONE)) {
			uint cur_dist = DistanceManhattan(tile, wp->xy);

			if (cur_dist < thres) {
				thres = cur_dist;
				best = wp;
			}
		}
	}

	return best;
}

/** Convert existing rail to waypoint. Eg build a waypoint station over
 * piece of rail
 * @param tile tile where waypoint will be built
 * @param flags type of operation
 * @param p1 graphics for waypoint type, 0 indicates standard graphics
 * @param p2 unused
 * @param text unused
 * @return cost of operation or error
 *
 * @todo When checking for the tile slope,
 * distingush between "Flat land required" and "land sloped in wrong direction"
 */
CommandCost CmdBuildTrainWaypoint(TileIndex tile, DoCommandFlag flags, uint32 p1, uint32 p2, const char *text)
{
	Axis axis;

	/* if custom gfx are used, make sure it is within bounds */
	if (p1 >= GetNumCustomStations(STAT_CLASS_WAYP)) return CMD_ERROR;

	if (!IsTileType(tile, MP_RAILWAY) ||
			GetRailTileType(tile) != RAIL_TILE_NORMAL || (
				(axis = AXIS_X, GetTrackBits(tile) != TRACK_BIT_X) &&
				(axis = AXIS_Y, GetTrackBits(tile) != TRACK_BIT_Y)
			)) {
		return_cmd_error(STR_ERROR_NO_SUITABLE_RAILROAD_TRACK);
	}

	Owner owner = GetTileOwner(tile);
	if (!CheckOwnership(owner)) return CMD_ERROR;
	if (!EnsureNoVehicleOnGround(tile)) return CMD_ERROR;

	Slope tileh = GetTileSlope(tile, NULL);
	if (tileh != SLOPE_FLAT &&
			(!_settings_game.construction.build_on_slopes || IsSteepSlope(tileh) || !(tileh & (0x3 << axis)) || !(tileh & ~(0x3 << axis)))) {
		return_cmd_error(STR_ERROR_FLAT_LAND_REQUIRED);
	}

	if (MayHaveBridgeAbove(tile) && IsBridgeAbove(tile)) return_cmd_error(STR_ERROR_MUST_DEMOLISH_BRIDGE_FIRST);

	/* Check if there is an already existing, deleted, waypoint close to us that we can reuse. */
	Waypoint *wp = FindDeletedWaypointCloseTo(tile, STR_SV_STNAME_WAYPOINT);
	if (wp == NULL && !Waypoint::CanAllocateItem()) return_cmd_error(STR_ERROR_TOO_MANY_STATIONS_LOADING);

	if (flags & DC_EXEC) {
		if (wp == NULL) {
			wp = new Waypoint(tile);
		} else {
			/* Move existing (recently deleted) waypoint to the new location */

			/* First we update the destination for all vehicles that
			 * have the old waypoint in their orders. */
			Vehicle *v;
			FOR_ALL_TRAINS(v) {
				if (v->First() == v && v->current_order.IsType(OT_GOTO_WAYPOINT) &&
						v->dest_tile == wp->xy) {
					v->dest_tile = tile;
				}
			}

			wp->xy = tile;
			InvalidateWindowData(WC_WAYPOINT_VIEW, wp->index);
		}
		wp->owner = owner;

		bool reserved = HasBit(GetRailReservationTrackBits(tile), AxisToTrack(axis));
		MakeRailWaypoint(tile, owner, wp->index, axis, 0, GetRailType(tile));
		SetRailwayStationReservation(tile, reserved);
		MarkTileDirtyByTile(tile);

		SetCustomStationSpecIndex(tile, AllocateSpecToStation(GetCustomStationSpec(STAT_CLASS_WAYP, p1), wp, true));

		wp->delete_ctr = 0;
		wp->facilities |= FACIL_TRAIN;
		wp->build_date = _date;
		wp->string_id = STR_SV_STNAME_WAYPOINT;

		if (wp->town == NULL) MakeDefaultWaypointName(wp);

		wp->UpdateVirtCoord();
		YapfNotifyTrackLayoutChange(tile, AxisToTrack(axis));
	}

	return CommandCost(EXPENSES_CONSTRUCTION, _price.build_train_depot);
}

/**
 * Remove a waypoint
 * @param tile from which to remove waypoint
 * @param flags type of operation
 * @param justremove will indicate if it is removed from rail or if rails are removed too
 * @pre IsRailWaypointTile(tile)
 * @return cost of operation or error
 */
CommandCost RemoveTrainWaypoint(TileIndex tile, DoCommandFlag flags, bool justremove)
{
	/* Make sure it's a waypoint */
	if (!IsRailWaypointTile(tile) ||
			(!CheckTileOwnership(tile) && _current_company != OWNER_WATER) ||
			!EnsureNoVehicleOnGround(tile)) {
		return CMD_ERROR;
	}

	if (flags & DC_EXEC) {
		Track track = GetRailStationTrack(tile);
		Waypoint *wp = Waypoint::GetByTile(tile);

		wp->sign.MarkDirty();
		wp->facilities &= ~FACIL_TRAIN;

		Train *v = NULL;
		uint specindex = GetCustomStationSpecIndex(tile);
		if (justremove) {
			TrackBits tracks = GetRailStationTrackBits(tile);
			bool reserved = HasStationReservation(tile);
			MakeRailNormal(tile, wp->owner, tracks, GetRailType(tile));
			if (reserved) SetTrackReservation(tile, tracks);
			MarkTileDirtyByTile(tile);
		} else {
			if (HasStationReservation(tile)) {
				v = GetTrainForReservation(tile, track);
				if (v != NULL) FreeTrainTrackReservation(v);
			}
			DoClearSquare(tile);
			AddTrackToSignalBuffer(tile, track, wp->owner);
		}
		YapfNotifyTrackLayoutChange(tile, track);
		if (v != NULL) TryPathReserve(v, true);

		DeallocateSpecFromStation(wp, specindex);
	}

	return CommandCost(EXPENSES_CONSTRUCTION, _price.remove_train_depot);
}

/**
 * Delete a waypoint
 * @param tile tile where waypoint is to be deleted
 * @param flags type of operation
 * @param p1 unused
 * @param p2 unused
 * @param text unused
 * @return cost of operation or error
 */
CommandCost CmdRemoveTrainWaypoint(TileIndex tile, DoCommandFlag flags, uint32 p1, uint32 p2, const char *text)
{
	return RemoveTrainWaypoint(tile, flags, true);
}


/** Build a buoy.
 * @param tile tile where to place the bouy
 * @param flags operation to perform
 * @param p1 unused
 * @param p2 unused
 * @param text unused
 * @return cost of operation or error
 */
CommandCost CmdBuildBuoy(TileIndex tile, DoCommandFlag flags, uint32 p1, uint32 p2, const char *text)
{
	if (!IsWaterTile(tile) || tile == 0) return_cmd_error(STR_ERROR_SITE_UNSUITABLE);
	if (MayHaveBridgeAbove(tile) && IsBridgeAbove(tile)) return_cmd_error(STR_ERROR_MUST_DEMOLISH_BRIDGE_FIRST);

	if (GetTileSlope(tile, NULL) != SLOPE_FLAT) return_cmd_error(STR_ERROR_SITE_UNSUITABLE);

	/* Check if there is an already existing, deleted, waypoint close to us that we can reuse. */
	Waypoint *wp = FindDeletedWaypointCloseTo(tile, STR_SV_STNAME_BUOY);
	if (wp == NULL && !Waypoint::CanAllocateItem()) return_cmd_error(STR_ERROR_TOO_MANY_STATIONS_LOADING);

	if (flags & DC_EXEC) {
		if (wp == NULL) {
			wp = new Waypoint(tile);
		} else {
			/* Move existing (recently deleted) buoy to the new location */
			wp->xy = tile;
			InvalidateWindowData(WC_WAYPOINT_VIEW, wp->index);
		}

		wp->string_id = STR_SV_STNAME_BUOY;

		wp->facilities |= FACIL_DOCK;
		wp->owner = OWNER_NONE;

		wp->build_date = _date;

		if (wp->town == NULL) MakeDefaultWaypointName(wp);

		MakeBuoy(tile, wp->index, GetWaterClass(tile));

		wp->UpdateVirtCoord();
		InvalidateWindowData(WC_WAYPOINT_VIEW, wp->index);
	}

	return CommandCost(EXPENSES_CONSTRUCTION, _price.build_dock);
}

/**
 * Remove a buoy
 * @param tile TileIndex been queried
 * @param flags operation to perform
 * @pre IsBuoyTile(tile)
 * @return cost or failure of operation
 */
CommandCost RemoveBuoy(TileIndex tile, DoCommandFlag flags)
{
	/* XXX: strange stuff, allow clearing as invalid company when clearing landscape */
	if (!Company::IsValidID(_current_company) && !(flags & DC_BANKRUPT)) return_cmd_error(INVALID_STRING_ID);

	Waypoint *wp = Waypoint::GetByTile(tile);

	if (HasStationInUse(wp->index, INVALID_COMPANY)) return_cmd_error(STR_BUOY_IS_IN_USE);
	/* remove the buoy if there is a ship on tile when company goes bankrupt... */
	if (!(flags & DC_BANKRUPT) && !EnsureNoVehicleOnGround(tile)) return CMD_ERROR;

	if (flags & DC_EXEC) {
		wp->facilities &= ~FACIL_DOCK;

		InvalidateWindowData(WC_WAYPOINT_VIEW, wp->index);

		/* We have to set the water tile's state to the same state as before the
		 * buoy was placed. Otherwise one could plant a buoy on a canal edge,
		 * remove it and flood the land (if the canal edge is at level 0) */
		MakeWaterKeepingClass(tile, GetTileOwner(tile));
		MarkTileDirtyByTile(tile);

		wp->UpdateVirtCoord();
		wp->delete_ctr = 0;
	}

	return CommandCost(EXPENSES_CONSTRUCTION, _price.remove_truck_station);
}


static bool IsUniqueWaypointName(const char *name)
{
	const Waypoint *wp;

	FOR_ALL_WAYPOINTS(wp) {
		if (wp->name != NULL && strcmp(wp->name, name) == 0) return false;
	}

	return true;
}

/**
 * Rename a waypoint.
 * @param tile unused
 * @param flags type of operation
 * @param p1 id of waypoint
 * @param p2 unused
 * @param text the new name of the waypoint or an empty string when resetting to the default
 * @return cost of operation or error
 */
CommandCost CmdRenameWaypoint(TileIndex tile, DoCommandFlag flags, uint32 p1, uint32 p2, const char *text)
{
	Waypoint *wp = Waypoint::GetIfValid(p1);
	if (wp == NULL || !(CheckOwnership(wp->owner) || wp->owner == OWNER_NONE)) return CMD_ERROR;

	bool reset = StrEmpty(text);

	if (!reset) {
		if (strlen(text) >= MAX_LENGTH_WAYPOINT_NAME_BYTES) return CMD_ERROR;
		if (!IsUniqueWaypointName(text)) return_cmd_error(STR_NAME_MUST_BE_UNIQUE);
	}

	if (flags & DC_EXEC) {
		free(wp->name);

		if (reset) {
			MakeDefaultWaypointName(wp); // sets wp->name = NULL
		} else {
			wp->name = strdup(text);
		}

		wp->UpdateVirtCoord();
	}
	return CommandCost();
}
