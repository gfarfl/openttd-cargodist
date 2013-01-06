/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file cargopacket.h Base class for cargo packets. */

#ifndef CARGOPACKET_H
#define CARGOPACKET_H

#include "core/pool_type.hpp"
#include "economy_type.h"
#include "station_type.h"
#include "cargo_type.h"
#include "vehicle_type.h"
#include <list>

/** Unique identifier for a single cargo packet. */
typedef uint32 CargoPacketID;
struct CargoPacket;

/** Type of the pool for cargo packets for a little over 16 million packets. */
typedef Pool<CargoPacket, CargoPacketID, 1024, 0xFFF000, PT_NORMAL, true, false> CargoPacketPool;
/** The actual pool with cargo packets. */
extern CargoPacketPool _cargopacket_pool;

template <class Tinst> class CargoList;
class StationCargoList; // forward-declare, so we can use it in VehicleCargoList.
extern const struct SaveLoad *GetCargoPacketDesc();

/**
 * Container for cargo from the same location and time.
 */
struct CargoPacket : CargoPacketPool::PoolItem<&_cargopacket_pool> {
private:
	Money feeder_share;         ///< Value of feeder pickup to be paid for on delivery of cargo.
	uint16 count;               ///< The amount of cargo in this packet.
	byte days_in_transit;       ///< Amount of days this packet has been in transit.
	SourceTypeByte source_type; ///< Type of \c source_id.
	SourceID source_id;         ///< Index of source, INVALID_SOURCE if unknown/invalid.
	StationID source;           ///< The station where the cargo came from first.
	TileIndex source_xy;        ///< The origin of the cargo (first station in feeder chain).
	TileIndex loaded_at_xy;     ///< Location where this cargo has been loaded into the vehicle.

	/** The CargoList caches, thus needs to know about it. */
	template <class Tinst> friend class CargoList;
	friend class VehicleCargoList;
	friend class StationCargoList;
	/** We want this to be saved, right? */
	friend const struct SaveLoad *GetCargoPacketDesc();
public:
	/** Maximum number of items in a single cargo packet. */
	static const uint16 MAX_COUNT = UINT16_MAX;

	CargoPacket();
	CargoPacket(StationID source, TileIndex source_xy, uint16 count, SourceType source_type, SourceID source_id);
	CargoPacket(uint16 count, byte days_in_transit, StationID source, TileIndex source_xy, TileIndex loaded_at_xy, Money feeder_share = 0, SourceType source_type = ST_INDUSTRY, SourceID source_id = INVALID_SOURCE);

	/** Destroy the packet. */
	~CargoPacket() { }

	CargoPacket *Split(uint new_size);
	void Merge(CargoPacket *cp);
	void Reduce(uint count);

	/**
	 * Sets the tile where the packet was loaded last.
	 * @param load_place Tile where the packet was loaded last.
	 */
	void SetLoadPlace(TileIndex load_place) { this->loaded_at_xy = load_place; }
	/**
	 * Adds some feeder share to the packet.
	 * @param new_share Feeder share to be added.
	 */
	void AddFeederShare(Money new_share) { this->feeder_share += new_share; }

	/**
	 * Gets the number of 'items' in this packet.
	 * @return Item count.
	 */
	inline uint16 Count() const
	{
		return this->count;
	}

	/**
	 * Gets the amount of money already paid to earlier vehicles in
	 * the feeder chain.
	 * @return Feeder share.
	 */
	inline Money FeederShare() const
	{
		return this->feeder_share;
	}

	/**
	 * Gets the number of days this cargo has been in transit.
	 * This number isn't really in days, but in 2.5 days (CARGO_AGING_TICKS = 185 ticks) and
	 * it is capped at 255.
	 * @return Length this cargo has been in transit.
	 */
	inline byte DaysInTransit() const
	{
		return this->days_in_transit;
	}

	/**
	 * Gets the type of the cargo's source. industry, town or head quarter.
	 * @return Source type.
	 */
	inline SourceType SourceSubsidyType() const
	{
		return this->source_type;
	}

	/**
	 * Gets the ID of the cargo's source. An IndustryID, TownID or CompanyID.
	 * @return Source ID.
	 */
	inline SourceID SourceSubsidyID() const
	{
		return this->source_id;
	}

	/**
	 * Gets the ID of the station where the cargo was loaded for the first time.
	 * @return StationID.
	 */
	inline SourceID SourceStation() const
	{
		return this->source;
	}

	/**
	 * Gets the coordinates of the cargo's source station.
	 * @return Source station's coordinates.
	 */
	inline TileIndex SourceStationXY() const
	{
		return this->source_xy;
	}

	/**
	 * Gets the coordinates of the cargo's last loading station.
	 * @return Last loading station's coordinates.
	 */
	inline TileIndex LoadedAtXY() const
	{
		return this->loaded_at_xy;
	}


	static void InvalidateAllFrom(SourceType src_type, SourceID src);
	static void InvalidateAllFrom(StationID sid);
	static void AfterLoad();
};

/**
 * Iterate over all _valid_ cargo packets from the given start.
 * @param var   Variable used as "iterator".
 * @param start Cargo packet ID of the first packet to iterate over.
 */
#define FOR_ALL_CARGOPACKETS_FROM(var, start) FOR_ALL_ITEMS_FROM(CargoPacket, cargopacket_index, var, start)

/**
 * Iterate over all _valid_ cargo packets from the begin of the pool.
 * @param var   Variable used as "iterator".
 */
#define FOR_ALL_CARGOPACKETS(var) FOR_ALL_CARGOPACKETS_FROM(var, 0)

/**
 * Simple collection class for a list of cargo packets.
 * @tparam Tinst Actual instantiation of this cargo list.
 */
template <class Tinst>
class CargoList {
public:
	/** Container with cargo packets. */
	typedef std::list<CargoPacket *> List;
	/** The iterator for our container. */
	typedef List::iterator Iterator;
	/** The reverse iterator for our container. */
	typedef List::reverse_iterator ReverseIterator;
	/** The const iterator for our container. */
	typedef List::const_iterator ConstIterator;

protected:
	uint count;                 ///< Cache for the number of cargo entities.
	uint cargo_days_in_transit; ///< Cache for the sum of number of days in transit of each entity; comparable to man-hours.

	List packets;               ///< The cargo packets in this list.

	void AddToCache(const CargoPacket *cp);

	void RemoveFromCache(const CargoPacket *cp, uint count);

	template<class Taction>
	void ShiftCargo(Taction action);

	template<class Taction>
	void PopCargo(Taction action);

	static bool TryMerge(CargoPacket *cp, CargoPacket *icp);

public:
	/** Create the cargo list. */
	CargoList() {}

	~CargoList();

	void OnCleanPool();

	/**
	 * Returns a pointer to the cargo packet list (so you can iterate over it etc).
	 * @return Pointer to the packet list.
	 */
	inline const List *Packets() const
	{
		return &this->packets;
	}

	/**
	 * Checks whether this list is empty.
	 * @return True if and only if the list is empty.
	 */
	inline bool Empty() const
	{
		return this->count == 0;
	}

	/**
	 * Returns the number of cargo entities in this list.
	 * @return The before mentioned number.
	 */
	inline uint Count() const
	{
		return this->count;
	}

	/**
	 * Returns source of the first cargo packet in this list.
	 * @return The before mentioned source.
	 */
	inline StationID Source() const
	{
		return this->Empty() ? INVALID_STATION : this->packets.front()->source;
	}

	/**
	 * Returns average number of days in transit for a cargo entity.
	 * @return The before mentioned number.
	 */
	inline uint DaysInTransit() const
	{
		return this->count == 0 ? 0 : this->cargo_days_in_transit / this->count;
	}


	void Append(CargoPacket *cp);
	void Truncate(uint max_remaining);

	void InvalidateCache();
};

/**
 * CargoList that is used for vehicles.
 */
class VehicleCargoList : public CargoList<VehicleCargoList> {
public:
	/**
	 * Action to be performed with a share of cargo when loading/unloading at
	 * the current station.
	 */
	enum Action {
		A_BEGIN = 0,
		A_TRANSFER = 0, ///< Transfer the cargo to the station.
		A_DELIVER,      ///< Deliver the cargo to some town or industry.
		A_KEEP,         ///< Keep the cargo in the vehicle.
		A_LOAD,         ///< Load the cargo from the station.
		A_END,
		NUM_ACTION = A_END
	};

protected:
	/** The (direct) parent of this class. */
	typedef CargoList<VehicleCargoList> Parent;

	Money feeder_share;                     ///< Cache for the feeder share.
	uint action_counts[NUM_ACTION]; ///< Counts of cargo to be transfered, delivered, kept and loaded.

	void AddToCache(const CargoPacket *cp);
	void RemoveFromCache(const CargoPacket *cp, uint count);

	/**
	 * Assert that the designation counts add up.
	 */
	inline void AssertCountConsistence() const
	{
		assert(this->action_counts[A_KEEP] +
				this->action_counts[A_DELIVER] +
				this->action_counts[A_TRANSFER] +
				this->action_counts[A_LOAD] == this->count);
	}

public:
	/** The super class ought to know what it's doing. */
	friend class CargoList<VehicleCargoList>;
	/** The vehicles have a cargo list (and we want that saved). */
	friend const struct SaveLoad *GetVehicleDescription(VehicleType vt);

	void Append(CargoPacket *cp, Action action = A_KEEP);

	void RemoveFromMeta(const CargoPacket *cp, Action action, uint count);
	void AddToMeta(const CargoPacket *cp, Action action);

	/**
	 * Moves some cargo from one designation to another. You can only move
	 * between adjacent designation. E.g. you can keep cargo that was
	 * previously reserved (MTA_LOAD) or you can mark cargo to be transferred
	 * that was previously marked as to be delivered, but you can't reserve
	 * cargo that's marked as to be delivered.
	 */
	inline void Reassign(uint count, Action from, Action to)
	{
		assert(Delta((int)from, (int)to) == 1);
		this->action_counts[from] -= count;
		this->action_counts[to] += count;
	}

	/**
	 * Marks all cargo in the vehicle as to be kept. This is only useful for
	 * loading old savegames. When loading is aborted the reserved cargo has
	 * to be returned.
	 */
	inline void KeepAll()
	{
		this->action_counts[A_DELIVER] = this->action_counts[A_TRANSFER] = this->action_counts[A_LOAD] = 0;
		this->action_counts[A_KEEP] = this->count;
	}

	/**
	 * Returns total sum of the feeder share for all packets.
	 * @return The before mentioned number.
	 */
	inline Money FeederShare() const
	{
		return this->feeder_share;
	}

	/**
	 * Returns the amount of cargo designated for a given purpose.
	 * @param action Action the cargo is designated for.
	 * @return Amount of cargo designated for the given action.
	 */
	inline uint ActionCount(Action action) const
	{
		return this->action_counts[action];
	}

	/**
	 * Returns sum of cargo on board the vehicle (ie not only
	 * reserved).
	 * @return Cargo on board the vehicle.
	 */
	inline uint OnboardCount() const
	{
		return this->count - this->action_counts[A_LOAD];
	}

	/**
	 * Returns sum of cargo to be moved out of the vehicle at the current station.
	 * @return Cargo to be moved.
	 */
	inline uint UnloadCount() const
	{
		return this->action_counts[A_TRANSFER] + this->action_counts[A_DELIVER];
	}

	/**
	 * Returns the sum of cargo to be kept in the vehicle at the current station.
	 * @return Cargo to be kept or loaded.
	 */
	inline uint RemainingCount() const
	{
		return this->action_counts[A_KEEP] + this->action_counts[A_LOAD];
	}

	uint Return(StationCargoList *dest, uint count = UINT_MAX);

	uint Unload(StationCargoList *dest, uint count, CargoPayment *payment);

	uint Shift(VehicleCargoList *dest, uint count);

	void AgeCargo();

	bool Stage(bool accepted, StationID current_station, uint8 order_flags);

	void InvalidateCache();

	/**
	 * Are two the two CargoPackets mergeable in the context of
	 * a list of CargoPackets for a Vehicle?
	 * @param cp1 First CargoPacket.
	 * @param cp2 Second CargoPacket.
	 * @return True if they are mergeable.
	 */
	static bool AreMergable(const CargoPacket *cp1, const CargoPacket *cp2)
	{
		return cp1->source_xy    == cp2->source_xy &&
				cp1->days_in_transit == cp2->days_in_transit &&
				cp1->source_type     == cp2->source_type &&
				cp1->source_id       == cp2->source_id &&
				cp1->loaded_at_xy    == cp2->loaded_at_xy;
	}
};

/**
 * CargoList that is used for stations.
 */
class StationCargoList : public CargoList<StationCargoList> {
protected:
	uint reserved_count; ///< Amount of cargo being reserved for loading.

public:
	/** The super class ought to know what it's doing. */
	friend class CargoList<StationCargoList>;
	/** The stations, via GoodsEntry, have a CargoList. */
	friend const struct SaveLoad *GetGoodsDesc();

	/**
	 * Are two the two CargoPackets mergeable in the context of
	 * a list of CargoPackets for a station?
	 * @param cp1 First CargoPacket.
	 * @param cp2 Second CargoPacket.
	 * @return True if they are mergeable.
	 */
	static bool AreMergable(const CargoPacket *cp1, const CargoPacket *cp2)
	{
		return cp1->source_xy    == cp2->source_xy &&
				cp1->days_in_transit == cp2->days_in_transit &&
				cp1->source_type     == cp2->source_type &&
				cp1->source_id       == cp2->source_id;
	}

	/**
	 * Returns sum of cargo reserved for loading onto vehicles.
	 * @return Cargo reserved for loading.
	 */
	inline uint ReservedCount() const
	{
		return this->reserved_count;
	}

	/**
	 * Returns total count of cargo, including reserved cargo that's not
	 * actually in the list.
	 * @return Total cargo count.
	 */
	inline uint TotalCount() const
	{
		return this->count + this->reserved_count;
	}

	/**
	 * Returns a previously reserved packet to the station it came from.
	 * @param cp Packet being returned.
	 */
	inline void Return(CargoPacket *cp)
	{
		assert(cp->count <= this->reserved_count);
		this->reserved_count -= cp->count;
		this->Append(cp);
	}

	/**
	 * Loads some reserved cargo onto the vehicle which had reserved it.
	 * @param move Amount of cargo being loaded.
	 */
	inline void LoadReserved(uint move)
	{
		assert(move <= this->reserved_count);
		this->reserved_count -= move;
	}

	/**
	 * Reserves some cargo for loading. It's assumed that the packet has
	 * already been removed from the list and only the metadata has to be
	 * updated.
	 * @param cp Packet being reserved.
	 */
	inline void Reserve(CargoPacket *cp)
	{
		this->reserved_count += cp->count;
		this->RemoveFromCache(cp, cp->count);
	}

	/**
	 * Load a packet onto a vehicle without reserving it before. It's assumed
	 * that the packet has already been removed from the list and only the
	 * cache has to be updated.
	 * @param cp Packet being loaded.
	 */
	inline void Load(CargoPacket *cp)
	{
		this->RemoveFromCache(cp, cp->count);
	}

	uint Reserve(VehicleCargoList *dest, uint count, TileIndex load_place);
	uint Load(VehicleCargoList *dest, uint count, TileIndex load_place);
};

/** Abstract action for moving cargo from one list to another. */
template<class Tsource, class Tdest>
class CargoMovement {
protected:
	Tsource *source;    ///< Source of the cargo.
	Tdest *destination; ///< Destination for the cargo.
	uint max_move;      ///< Maximum amount of cargo to be moved with this action.
	CargoPacket *Preprocess(CargoPacket *cp);
public:
	CargoMovement(Tsource *source, Tdest *destination, uint max_move) : source(source), destination(destination), max_move(max_move) {}

	/**
	 * Returns how much more cargo can be moved with this action.
	 * @return Amount of cargo this action can still move.
	 */
	uint MaxMove() { return this->max_move; }
};

/** Action of final delivery of cargo. */
class CargoDelivery {
private:
	VehicleCargoList *source; ///< Source of the cargo.
	CargoPayment *payment;    ///< Payment object where payments will be registered.
	uint max_move;            ///< Maximum amount of cargo to be delivered with this action.
public:
	CargoDelivery(VehicleCargoList *source, uint max_move, CargoPayment *payment) : source(source), payment(payment), max_move(max_move) {}
	bool operator()(CargoPacket *cp);

	/**
	 * Returns how much more cargo can be delivered with this action.
	 * @return Amount of cargo this action can still deliver.
	 */
	uint MaxMove() { return this->max_move; }
};

/** Action of transferring cargo from a vehicle to a station. */
class CargoTransfer : public CargoMovement<VehicleCargoList, StationCargoList> {
protected:
	CargoPayment *payment; ///< Payment object for registering transfer credits.
public:
	CargoTransfer(VehicleCargoList *source, StationCargoList *destination, uint max_move, CargoPayment *payment) :
			CargoMovement<VehicleCargoList, StationCargoList>(source, destination, max_move), payment(payment) {}
	bool operator()(CargoPacket *cp);
};

/** Action of loading cargo from a station onto a vehicle. */
class CargoLoad : public CargoMovement<StationCargoList, VehicleCargoList> {
protected:
	TileIndex load_place; ///< TileIndex to be saved in the packets' loaded_at_xy.
public:
	CargoLoad(StationCargoList *source, VehicleCargoList *destination, uint max_move, TileIndex load_place) :
			CargoMovement<StationCargoList, VehicleCargoList>(source, destination, max_move), load_place(load_place) {}
	bool operator()(CargoPacket *cp);
};

/** Action of reserving cargo from a station to be loaded onto a vehicle. */
class CargoReservation : public CargoLoad {
public:
	CargoReservation(StationCargoList *source, VehicleCargoList *destination, uint max_move, TileIndex load_place) :
			CargoLoad(source, destination, max_move, load_place) {}
	bool operator()(CargoPacket *cp);
};

/** Action of returning previously reserved cargo from the vehicle to the station. */
class CargoReturn : public CargoMovement<VehicleCargoList, StationCargoList> {
public:
	CargoReturn(VehicleCargoList *source, StationCargoList *destination, uint max_move) :
			CargoMovement<VehicleCargoList, StationCargoList>(source, destination, max_move) {}
	bool operator()(CargoPacket *cp);
};

/** Action of shifting cargo from one vehicle to another. */
class CargoShift : public CargoMovement<VehicleCargoList, VehicleCargoList> {
public:
	CargoShift(VehicleCargoList *source, VehicleCargoList *destination, uint max_move) :
			CargoMovement<VehicleCargoList, VehicleCargoList>(source, destination, max_move) {}
	bool operator()(CargoPacket *cp);
};

#endif /* CARGOPACKET_H */
