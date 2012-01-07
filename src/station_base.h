/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file station_base.h Base classes/functions for stations. */

#ifndef STATION_BASE_H
#define STATION_BASE_H

#include "base_station_base.h"
#include "newgrf_airport.h"
#include "cargopacket.h"
#include "industry_type.h"
#include "linkgraph/linkgraph_type.h"
#include "newgrf_storage.h"
#include <map>

typedef Pool<BaseStation, StationID, 32, 64000> StationPool;
extern StationPool _station_pool;

static const byte INITIAL_STATION_RATING = 175;

/**
 * Link statistics. They include figures for capacity and usage of a link. Both
 * are moving averages which are increased for every vehicle arriving at the
 * destination station and decreased by <current value>/(distance + 1) once a
 * game day. The timeout is decreased by 1/49th once a game day. When a vehicle
 * arrives it gets reset to the value of distance. When it reaches 0 the link
 * is considered dead. As long as the link is not dead the capacity is kept at
 * a minimum of 1. This is so that the death of a link is not dependent on
 * its capacity but only on its distance. Otherwise some infrequently visited
 * long distance links would be impossible to keep alive.
 * Additionally while a vehicle is loading at the source station part of the
 * capacity is frozen and prevented from being further decreased. This is done
 * so that the link won't break down all the time when the typical "full load"
 * order is used.
 */
class LinkStat {
private:
	/**
	 * Manhattan distance between end points of the link.
	 */
	uint distance;

	/**
	 * Capacity of the link.
	 * This is a moving average. Use Capacity() to get a meaningful value.
	 */
	uint capacity;

	/**
	 * Time until the link is removed. Decreases exponentially.
	 */
	uint timeout;

	/**
	 * Usage of the link.
	 * This is a moving average. Use Usage() to get a meaningful value.
	 */
	uint usage;

public:
	static const uint MIN_DISTANCE = 48;          ///< Minimum distance for moving averages.
	static const uint MOVING_AVERAGE_LENGTH = 30; ///< Length of moving average in days.

	friend const SaveLoad *GetLinkStatDesc();

	/**
	 * We don't allow creating a link stat without a timeout/length.
	 */
	LinkStat() {NOT_REACHED();}

	/**
	 * Create a link stat with at least a distance.
	 * @param distance Length for the moving average and link timeout.
	 * @param capacity Initial capacity of the link.
	 * @param usage Initial usage of the link.
	 */
	inline LinkStat(uint distance, uint capacity = 1, uint usage = 0) :
		distance(distance), capacity(capacity), timeout(distance), usage(usage)
	{
		assert(this->distance > 0);
		assert(this->usage <= this->capacity);
	}

	/**
	 * Reset everything to 0.
	 */
	inline void Clear()
	{
		this->capacity = 1;
		this->usage = 0;
		this->timeout = this->distance;
	}

	/**
	 * Apply the moving averages to usage and capacity.
	 */
	inline void Decrease()
	{
		this->usage = this->usage * this->distance / (this->distance + 1);
		this->timeout = this->timeout * MIN_DISTANCE / (MIN_DISTANCE + 1);
		this->capacity = max(this->capacity * this->distance / (this->distance + 1), (uint)1);
		assert(this->usage <= this->capacity);
	}

	/**
	 * Get an estimate of the current the capacity by calculating the moving average.
	 * @return Capacity.
	 */
	inline uint Capacity() const
	{
		return this->capacity * LinkStat::MOVING_AVERAGE_LENGTH / this->distance;
	}

	/**
	 * Get an estimage of the current usage by calculating the moving average.
	 * @return Usage.
	 */
	inline uint Usage() const
	{
		return this->usage * LinkStat::MOVING_AVERAGE_LENGTH / this->distance;
	}

	/**
	 * Add some capacity and usage.
	 * @param capacity Additional capacity.
	 * @param usage Additional usage.
	 */
	inline void Increase(uint capacity, uint usage)
	{
		this->timeout = this->distance;
		this->capacity += capacity;
		this->usage += usage;
		assert(this->usage <= this->capacity);
	}

	/**
	 * Reset the timeout and make sure there is at least a minimum capacity.
	 */
	inline void Refresh(uint min_capacity)
	{
		this->capacity = max(this->capacity, min_capacity);
		this->timeout = this->distance;
	}

	/**
	 * Check if the timeout has hit.
	 * @return If timeout is > 0.
	 */
	inline bool IsValid() const
	{
		return this->timeout > 0;
	}
};

typedef std::map<StationID, LinkStat> LinkStatMap;

uint GetMovingAverageLength(const Station *from, const Station *to);

/**
 * Stores station stats for a single cargo.
 */
struct GoodsEntry {
	/** Status of this cargo for the station. */
	enum GoodsEntryStatus {
		/**
		 * Set when the station accepts the cargo currently for final deliveries.
		 * It is updated every STATION_ACCEPTANCE_TICKS ticks by checking surrounding tiles for acceptance >= 8/8.
		 */
		GES_ACCEPTANCE,

		/**
		 * Set when the cargo was ever waiting at the station.
		 * It is set when cargo supplied by surrounding tiles is moved to the station, or when
		 * arriving vehicles unload/transfer cargo without it being a final delivery.
		 * This also indicates, whether a cargo has a rating at the station.
		 * This flag is never cleared.
		 */
		GES_PICKUP,

		/**
		 * Set when a vehicle ever delivered cargo to the station for final delivery.
		 * This flag is never cleared.
		 */
		GES_EVER_ACCEPTED,

		/**
		 * Set when cargo was delivered for final delivery last month.
		 * This flag is set to the value of GES_CURRENT_MONTH at the start of each month.
		 */
		GES_LAST_MONTH,

		/**
		 * Set when cargo was delivered for final delivery this month.
		 * This flag is reset on the beginning of every month.
		 */
		GES_CURRENT_MONTH,

		/**
		 * Set when cargo was delivered for final delivery during the current STATION_ACCEPTANCE_TICKS interval.
		 * This flag is reset every STATION_ACCEPTANCE_TICKS ticks.
		 */
		GES_ACCEPTED_BIGTICK,
	};

	GoodsEntry() :
		acceptance_pickup(0),
		time_since_pickup(255),
		rating(INITIAL_STATION_RATING),
		last_speed(0),
		last_age(255),
		supply(0),
		supply_new(0),
		last_component(INVALID_LINKGRAPH_COMPONENT)
	{}

	byte acceptance_pickup; ///< Status of this cargo, see #GoodsEntryStatus.

	/**
	 * Number of rating-intervals (up to 255) since the last vehicle tried to load this cargo.
	 * The unit used is STATION_RATING_TICKS.
	 * This does not imply there was any cargo to load.
	 */
	byte time_since_pickup;

	byte rating;            ///< %Station rating for this cargo.

	/**
	 * Maximum speed (up to 255) of the last vehicle that tried to load this cargo.
	 * This does not imply there was any cargo to load.
	 * The unit used is a special vehicle-specific speed unit for station ratings.
	 *  - Trains: km-ish/h
	 *  - RV: km-ish/h
	 *  - Ships: 0.5 * km-ish/h
	 *  - Aircraft: 8 * mph
	 */
	byte last_speed;

	/**
	 * Age in years (up to 255) of the last vehicle that tried to load this cargo.
	 * This does not imply there was any cargo to load.
	 */
	byte last_age;

	byte amount_fract;      ///< Fractional part of the amount in the cargo list
	StationCargoList cargo; ///< The cargo packets of cargo waiting in this station

	uint supply;            ///< Cargo supplied last month.
	uint supply_new;        ///< Cargo supplied so far this month.
	LinkStatMap link_stats; ///< Capacities and usage statistics for outgoing links.

	LinkGraphComponentID last_component; ///< Component this station was last part of in this cargo's link graph.

	/**
	 * Reports whether a vehicle has ever tried to load the cargo at this station.
	 * This does not imply that there was cargo available for loading. Refer to GES_PICKUP for that.
	 * @return true if vehicle tried to load.
	 */
	bool HasVehicleEverTriedLoading() const { return this->last_speed != 0; }
};

/** All airport-related information. Only valid if tile != INVALID_TILE. */
struct Airport : public TileArea {
	Airport() : TileArea(INVALID_TILE, 0, 0) {}

	uint64 flags;       ///< stores which blocks on the airport are taken. was 16 bit earlier on, then 32
	byte type;          ///< Type of this airport, @see AirportTypes
	byte layout;        ///< Airport layout number.
	Direction rotation; ///< How this airport is rotated.

	PersistentStorage *psa; ///< Persistent storage for NewGRF airports.

	/**
	 * Get the AirportSpec that from the airport type of this airport. If there
	 * is no airport (\c tile == INVALID_TILE) then return the dummy AirportSpec.
	 * @return The AirportSpec for this airport.
	 */
	const AirportSpec *GetSpec() const
	{
		if (this->tile == INVALID_TILE) return &AirportSpec::dummy;
		return AirportSpec::Get(this->type);
	}

	/**
	 * Get the finite-state machine for this airport or the finite-state machine
	 * for the dummy airport in case this isn't an airport.
	 * @pre this->type < NEW_AIRPORT_OFFSET.
	 * @return The state machine for this airport.
	 */
	const AirportFTAClass *GetFTA() const
	{
		return this->GetSpec()->fsm;
	}

	/** Check if this airport has at least one hangar. */
	inline bool HasHangar() const
	{
		return this->GetSpec()->nof_depots > 0;
	}

	/**
	 * Add the tileoffset to the base tile of this airport but rotate it first.
	 * The base tile is the northernmost tile of this airport. This function
	 * helps to make sure that getting the tile of a hangar works even for
	 * rotated airport layouts without requiring a rotated array of hangar tiles.
	 * @param tidc The tilediff to add to the airport tile.
	 * @return The tile of this airport plus the rotated offset.
	 */
	inline TileIndex GetRotatedTileFromOffset(TileIndexDiffC tidc) const
	{
		const AirportSpec *as = this->GetSpec();
		switch (this->rotation) {
			case DIR_N: return this->tile + ToTileIndexDiff(tidc);

			case DIR_E: return this->tile + TileDiffXY(tidc.y, as->size_x - 1 - tidc.x);

			case DIR_S: return this->tile + TileDiffXY(as->size_x - 1 - tidc.x, as->size_y - 1 - tidc.y);

			case DIR_W: return this->tile + TileDiffXY(as->size_y - 1 - tidc.y, tidc.x);

			default: NOT_REACHED();
		}
	}

	/**
	 * Get the first tile of the given hangar.
	 * @param hangar_num The hangar to get the location of.
	 * @pre hangar_num < GetNumHangars().
	 * @return A tile with the given hangar.
	 */
	inline TileIndex GetHangarTile(uint hangar_num) const
	{
		const AirportSpec *as = this->GetSpec();
		for (uint i = 0; i < as->nof_depots; i++) {
			if (as->depot_table[i].hangar_num == hangar_num) {
				return this->GetRotatedTileFromOffset(as->depot_table[i].ti);
			}
		}
		NOT_REACHED();
	}

	/**
	 * Get the exit direction of the hangar at a specific tile.
	 * @param tile The tile to query.
	 * @pre IsHangarTile(tile).
	 * @return The exit direction of the hangar, taking airport rotation into account.
	 */
	inline Direction GetHangarExitDirection(TileIndex tile) const
	{
		const AirportSpec *as = this->GetSpec();
		const HangarTileTable *htt = GetHangarDataByTile(tile);
		return ChangeDir(htt->dir, DirDifference(this->rotation, as->rotation[0]));
	}

	/**
	 * Get the hangar number of the hangar at a specific tile.
	 * @param tile The tile to query.
	 * @pre IsHangarTile(tile).
	 * @return The hangar number of the hangar at the given tile.
	 */
	inline uint GetHangarNum(TileIndex tile) const
	{
		const HangarTileTable *htt = GetHangarDataByTile(tile);
		return htt->hangar_num;
	}

	/** Get the number of hangars on this airport. */
	inline uint GetNumHangars() const
	{
		uint num = 0;
		uint counted = 0;
		const AirportSpec *as = this->GetSpec();
		for (uint i = 0; i < as->nof_depots; i++) {
			if (!HasBit(counted, as->depot_table[i].hangar_num)) {
				num++;
				SetBit(counted, as->depot_table[i].hangar_num);
			}
		}
		return num;
	}

private:
	/**
	 * Retrieve hangar information of a hangar at a given tile.
	 * @param tile %Tile containing the hangar.
	 * @return The requested hangar information.
	 * @pre The \a tile must be at a hangar tile at an airport.
	 */
	inline const HangarTileTable *GetHangarDataByTile(TileIndex tile) const
	{
		const AirportSpec *as = this->GetSpec();
		for (uint i = 0; i < as->nof_depots; i++) {
			if (this->GetRotatedTileFromOffset(as->depot_table[i].ti) == tile) {
				return as->depot_table + i;
			}
		}
		NOT_REACHED();
	}
};

typedef SmallVector<Industry *, 2> IndustryVector;

/** Station data structure */
struct Station FINAL : SpecializedStation<Station, false> {
public:
	RoadStop *GetPrimaryRoadStop(RoadStopType type) const
	{
		return type == ROADSTOP_BUS ? bus_stops : truck_stops;
	}

	RoadStop *GetPrimaryRoadStop(const struct RoadVehicle *v) const;

	RoadStop *bus_stops;    ///< All the road stops
	TileArea bus_station;   ///< Tile area the bus 'station' part covers
	RoadStop *truck_stops;  ///< All the truck stops
	TileArea truck_station; ///< Tile area the truck 'station' part covers

	Airport airport;        ///< Tile area the airport covers
	TileIndex dock_tile;    ///< The location of the dock

	IndustryType indtype;   ///< Industry type to get the name from

	StationHadVehicleOfTypeByte had_vehicle_of_type;

	byte time_since_load;
	byte time_since_unload;

	byte last_vehicle_type;
	std::list<Vehicle *> loading_vehicles;
	GoodsEntry goods[NUM_CARGO];  ///< Goods at this station
	uint32 always_accepted;       ///< Bitmask of always accepted cargo types (by houses, HQs, industry tiles when industry doesn't accept cargo)

	IndustryVector industries_near; ///< Cached list of industries near the station that can accept cargo, @see DeliverGoodsToIndustry()

	Station(TileIndex tile = INVALID_TILE);
	~Station();

	void AddFacility(StationFacility new_facility_bit, TileIndex facil_xy);

	void MarkTilesDirty(bool cargo_change) const;

	void UpdateVirtCoord();

	/* virtual */ uint GetPlatformLength(TileIndex tile, DiagDirection dir) const;
	/* virtual */ uint GetPlatformLength(TileIndex tile) const;
	void RecomputeIndustriesNear();
	static void RecomputeIndustriesNearForAll();

	uint GetCatchmentRadius() const;
	Rect GetCatchmentRect() const;

	/* virtual */ inline bool TileBelongsToRailStation(TileIndex tile) const
	{
		return IsRailStationTile(tile) && GetStationIndex(tile) == this->index;
	}

	inline bool TileBelongsToAirport(TileIndex tile) const
	{
		return IsAirportTile(tile) && GetStationIndex(tile) == this->index;
	}

	/* virtual */ uint32 GetNewGRFVariable(const ResolverObject *object, byte variable, byte parameter, bool *available) const;

	/* virtual */ void GetTileArea(TileArea *ta, StationType type) const;

	void RunAverages();
};

#define FOR_ALL_STATIONS(var) FOR_ALL_BASE_STATIONS_OF_TYPE(Station, var)

/** Iterator to iterate over all tiles belonging to an airport. */
class AirportTileIterator : public OrthogonalTileIterator {
private:
	const Station *st; ///< The station the airport is a part of.

public:
	/**
	 * Construct the iterator.
	 * @param ta Area, i.e. begin point and width/height of to-be-iterated area.
	 */
	AirportTileIterator(const Station *st) : OrthogonalTileIterator(st->airport), st(st)
	{
		if (!st->TileBelongsToAirport(this->tile)) ++(*this);
	}

	inline TileIterator& operator ++()
	{
		(*this).OrthogonalTileIterator::operator++();
		while (this->tile != INVALID_TILE && !st->TileBelongsToAirport(this->tile)) {
			(*this).OrthogonalTileIterator::operator++();
		}
		return *this;
	}

	virtual TileIterator *Clone() const
	{
		return new AirportTileIterator(*this);
	}
};

#endif /* STATION_BASE_H */
