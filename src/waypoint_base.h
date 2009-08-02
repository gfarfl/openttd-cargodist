/* $Id$ */

/** @file waypoint_base.h Base of waypoints. */

#ifndef WAYPOINT_H
#define WAYPOINT_H

#include "base_station_base.h"

struct Waypoint : SpecializedStation<Waypoint, true> {
	uint16 town_cn;    ///< The Nth waypoint for this town (consecutive number)

	Waypoint(TileIndex tile = INVALID_TILE) : SpecializedStation<Waypoint, true>(tile) { }
	~Waypoint();

	void UpdateVirtCoord();

	/* virtual */ FORCEINLINE bool TileBelongsToRailStation(TileIndex tile) const
	{
		return IsRailWaypointTile(tile) && GetStationIndex(tile) == this->index;
	}

	/* virtual */ uint32 GetNewGRFVariable(const struct ResolverObject *object, byte variable, byte parameter, bool *available) const;

	/* virtual */ void GetTileArea(TileArea *ta, StationType type) const;

	/* virtual */ uint GetPlatformLength(TileIndex tile, DiagDirection dir) const
	{
		return 1;
	}

	/* virtual */ uint GetPlatformLength(TileIndex tile) const
	{
		return 1;
	}
};

#define FOR_ALL_WAYPOINTS(var) FOR_ALL_BASE_STATIONS_OF_TYPE(Waypoint, var)

#endif /* WAYPOINT_H */
