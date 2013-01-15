/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file cargopacket.cpp Implementation of the cargo packets. */

#include "stdafx.h"
#include "station_base.h"
#include "core/pool_func.hpp"
#include "core/random_func.hpp"
#include "economy_base.h"
#include "order_type.h"

/* Initialize the cargopacket-pool */
CargoPacketPool _cargopacket_pool("CargoPacket");
INSTANTIATE_POOL_METHODS(CargoPacket)

/**
 * Create a new packet for savegame loading.
 */
CargoPacket::CargoPacket()
{
	this->source_type = ST_INDUSTRY;
	this->source_id   = INVALID_SOURCE;
}

/**
 * Creates a new cargo packet.
 * @param source      Source station of the packet.
 * @param source_xy   Source location of the packet.
 * @param count       Number of cargo entities to put in this packet.
 * @param source_type 'Type' of source the packet comes from (for subsidies).
 * @param source_id   Actual source of the packet (for subsidies).
 * @pre count != 0
 * @note We have to zero memory ourselves here because we are using a 'new'
 * that, in contrary to all other pools, does not memset to 0.
 */
CargoPacket::CargoPacket(StationID source, TileIndex source_xy, uint16 count, SourceType source_type, SourceID source_id) :
	feeder_share(0),
	count(count),
	days_in_transit(0),
	source_id(source_id),
	source(source),
	source_xy(source_xy),
	loaded_at_xy(0)
{
	assert(count != 0);
	this->source_type  = source_type;
}

/**
 * Creates a new cargo packet. Initializes the fields that cannot be changed later.
 * Used when loading or splitting packets.
 * @param count           Number of cargo entities to put in this packet.
 * @param days_in_transit Number of days the cargo has been in transit.
 * @param source          Station the cargo was initially loaded.
 * @param source_xy       Station location the cargo was initially loaded.
 * @param loaded_at_xy    Location the cargo was loaded last.
 * @param feeder_share    Feeder share the packet has already accumulated.
 * @param source_type     'Type' of source the packet comes from (for subsidies).
 * @param source_id       Actual source of the packet (for subsidies).
 * @note We have to zero memory ourselves here because we are using a 'new'
 * that, in contrary to all other pools, does not memset to 0.
 */
CargoPacket::CargoPacket(uint16 count, byte days_in_transit, StationID source, TileIndex source_xy, TileIndex loaded_at_xy, Money feeder_share, SourceType source_type, SourceID source_id) :
		feeder_share(feeder_share),
		count(count),
		days_in_transit(days_in_transit),
		source_id(source_id),
		source(source),
		source_xy(source_xy),
		loaded_at_xy(loaded_at_xy)
{
	assert(count != 0);
	this->source_type = source_type;
}

/**
 * Split this packet in two and return the split off part.
 * @param new_size Size of the split part.
 * @return Split off part, or NULL if no packet could be allocated!
 */
inline CargoPacket *CargoPacket::Split(uint new_size)
{
	if (!CargoPacket::CanAllocateItem()) return NULL;

	Money fs = this->feeder_share * new_size / static_cast<uint>(this->count);
	CargoPacket *cp_new = new CargoPacket(new_size, this->days_in_transit, this->source, this->source_xy, this->loaded_at_xy, fs, this->source_type, this->source_id);
	this->feeder_share -= fs;
	this->count -= new_size;
	return cp_new;
}

/**
 * Reduce the packet by the given amount and remove the feeder share.
 * @param count Amount to be removed.
 */
inline void CargoPacket::Reduce(uint count)
{
	this->feeder_share = 0;
	this->count -= count;
}

/**
 * Merge another packet into this one.
 * @param cp Packet to be merged in.
 */
inline void CargoPacket::Merge(CargoPacket *cp)
{
	this->count += cp->count;
	this->feeder_share += cp->feeder_share;
	delete cp;
}

/**
 * Invalidates (sets source_id to INVALID_SOURCE) all cargo packets from given source.
 * @param src_type Type of source.
 * @param src Index of source.
 */
/* static */ void CargoPacket::InvalidateAllFrom(SourceType src_type, SourceID src)
{
	CargoPacket *cp;
	FOR_ALL_CARGOPACKETS(cp) {
		if (cp->source_type == src_type && cp->source_id == src) cp->source_id = INVALID_SOURCE;
	}
}

/**
 * Invalidates (sets source to INVALID_STATION) all cargo packets from given station.
 * @param sid Station that gets removed.
 */
/* static */ void CargoPacket::InvalidateAllFrom(StationID sid)
{
	CargoPacket *cp;
	FOR_ALL_CARGOPACKETS(cp) {
		if (cp->source == sid) cp->source = INVALID_STATION;
	}
}

/*
 *
 * Cargo list implementation
 *
 */

/**
 * Destroy the cargolist ("frees" all cargo packets).
 */
template <class Tinst, class Tcont>
CargoList<Tinst, Tcont>::~CargoList()
{
	for (Iterator it(this->packets.begin()); it != this->packets.end(); ++it) {
		delete *it;
	}
}

/**
 * Empty the cargo list, but don't free the cargo packets;
 * the cargo packets are cleaned by CargoPacket's CleanPool.
 */
template <class Tinst, class Tcont>
void CargoList<Tinst, Tcont>::OnCleanPool()
{
	this->packets.clear();
}

/**
 * Update the cached values to reflect the removal of this packet or part of it.
 * Decreases count and days_in_transit.
 * @param cp Packet to be removed from cache.
 * @param count Amount of cargo from the given packet to be removed.
 */
template <class Tinst, class Tcont>
void CargoList<Tinst, Tcont>::RemoveFromCache(const CargoPacket *cp, uint count)
{
	assert(count <= cp->count);
	this->count                 -= count;
	this->cargo_days_in_transit -= cp->days_in_transit * count;
}

/**
 * Update the cache to reflect adding of this packet.
 * Increases count and days_in_transit.
 * @param cp New packet to be inserted.
 */
template <class Tinst, class Tcont>
void CargoList<Tinst, Tcont>::AddToCache(const CargoPacket *cp)
{
	this->count                 += cp->count;
	this->cargo_days_in_transit += cp->days_in_transit * cp->count;
}

/**
 * Truncates the cargo in this list to the given amount. It leaves the
 * first count cargo entities and removes the rest.
 * @param max_remaining Maximum amount of entities to be in the list after the command.
 */
template <class Tinst, class Tcont>
void CargoList<Tinst, Tcont>::Truncate(uint max_remaining)
{
	for (Iterator it(packets.begin()); it != packets.end(); /* done during loop*/) {
		CargoPacket *cp = *it;
		if (max_remaining == 0) {
			/* Nothing should remain, just remove the packets. */
			it = this->packets.erase(it);
			static_cast<Tinst *>(this)->RemoveFromCache(cp, cp->count);
			delete cp;
			continue;
		}

		uint local_count = cp->count;
		if (local_count > max_remaining) {
			uint diff = local_count - max_remaining;
			this->count -= diff;
			this->cargo_days_in_transit -= cp->days_in_transit * diff;
			cp->count = max_remaining;
			max_remaining = 0;
		} else {
			max_remaining -= local_count;
		}
		++it;
	}
}

/**
 * Tries to merge the second packet into the first and return if that was
 * successful.
 * @param icp Packet to be merged into.
 * @param cp Packet to be eliminated.
 * @return If the packets could be merged.
 */
template <class Tinst, class Tcont>
/* static */ bool CargoList<Tinst, Tcont>::TryMerge(CargoPacket *icp, CargoPacket *cp)
{
	if (Tinst::AreMergable(icp, cp) &&
			icp->count + cp->count <= CargoPacket::MAX_COUNT) {
		icp->Merge(cp);
		return true;
	} else {
		return false;
	}
}

/**
 * Appends the given cargo packet. Tries to merge it with another one in the
 * packets list. If no fitting packet is found, appends it. You can only append
 * packets to the ranges of packets designated for keeping or loading.
 * Furthermore if there are already packets reserved for loading you cannot
 * directly add packets to the "keep" list. You first have to load the reserved
 * ones.
 * @warning After appending this packet may not exist anymore!
 * @note Do not use the cargo packet anymore after it has been appended to this CargoList!
 * @param cp Cargo packet to add.
 * @param action Either MTA_KEEP if you want to add the packet directly or MTA_LOAD
 * if you want to reserve it first.
 * @pre cp != NULL
 * @pre action == MTA_LOAD || (action == MTA_KEEP && this->designation_counts[MTA_LOAD] == 0)
 */
void VehicleCargoList::Append(CargoPacket *cp, Action action)
{
	assert(cp != NULL);
	assert(action == A_LOAD ||
			(action == A_KEEP && this->action_counts[A_LOAD] == 0));
	this->AddToMeta(cp, action);

	if (this->count == cp->count) {
		this->packets.push_back(cp);
		return;
	}

	uint sum = cp->count;
	for (ReverseIterator it(this->packets.rbegin()); it != this->packets.rend(); it++) {
		CargoPacket *icp = *it;
		if (VehicleCargoList::TryMerge(icp, cp)) return;
		sum += icp->count;
		if (sum >= this->action_counts[action]) {
			this->packets.push_back(cp);
			return;
		}
	}

	NOT_REACHED();
}

/**
 * Returns reserved cargo to the station and removes it from the cache.
 * @param dest Station the cargo is returned to.
 * @param max_move Maximum amount of cargo to move.
 * @param ID of next the station the cargo wants to go next.
 * @return Amount of cargo actually returned.
 */
uint VehicleCargoList::Return(StationCargoList *dest, uint max_move, StationID next)
{
	max_move = min(this->action_counts[A_LOAD], max_move);
	this->PopCargo(CargoReturn(this, dest, max_move, next));
	return max_move;
}

/**
 * Loads cargo onto a vehicle. If the vehicle has reserved cargo load that.
 * Otherwise load cargo from the station.
 * @param dest Vehicle cargo list where the cargo resides.
 * @param max_move Amount of cargo to load.
 * @return Amount of cargo actually loaded.
 */
uint StationCargoList::Load(VehicleCargoList *dest, uint max_move, TileIndex load_place, StationID next_station)
{
	uint move = min(dest->ActionCount(VehicleCargoList::A_LOAD), max_move);
	if (move > 0) {
		this->reserved_count -= move;
		dest->Reassign(move, VehicleCargoList::A_LOAD, VehicleCargoList::A_KEEP);
	} else {
		move = min(this->count, max_move);
		this->ShiftCargo(CargoLoad(this, dest, move, load_place), next_station);
	}
	return move;
}

/**
 * Reserves cargo for loading onto the vehicle.
 * @param dest VehicleCargoList to reserve for.
 * @param max_move Maximum amount of cargo to reserve.
 * @param load_place Tile index of the current station.
 * @return Amount of cargo actually reserved.
 */
uint StationCargoList::Reserve(VehicleCargoList *dest, uint max_move, TileIndex load_place, StationID next)
{
	max_move = min(this->count, max_move);
	this->ShiftCargo(CargoReservation(this, dest, max_move, load_place), next);
	return max_move;
}

/**
 * Unloads cargo at the given station. Deliver or transfer, depending on the
 * ranges defined by designation_counts.
 * @param dest StationCargoList to add transferred cargo to.
 * @param max_move Maximum amount of cargo to move.
 * @param payment Payment object to register payments in.
 * @return Amount of cargo actually unloaded.
 */
uint VehicleCargoList::Unload(StationCargoList *dest, uint max_move, CargoPayment *payment)
{
	uint moved = 0;
	if (this->action_counts[A_TRANSFER] > 0) {
		uint move = min(this->action_counts[A_TRANSFER], max_move);
		this->ShiftCargo(CargoTransfer(this, dest, move, payment));
		moved += move;
	}
	if (this->action_counts[A_TRANSFER] == 0 && this->action_counts[A_DELIVER] > 0 && moved < max_move) {
		uint move = min(this->action_counts[A_DELIVER], max_move - moved);
		this->ShiftCargo(CargoDelivery(this, move, payment));
		moved += move;
	}
	return moved;
}

/**
 * Shifts cargo between two vehicles.
 * @param dest Other vehicle's cargo list.
 * @param max_move Maximum amount of cargo to be moved.
 * @return Amount of cargo actually moved.
 */
uint VehicleCargoList::Shift(VehicleCargoList *dest, uint max_move)
{
	max_move = max(this->count, max_move);
	this->PopCargo(CargoShift(this, dest, max_move));
	return max_move;
}

/**
 * Shift cargo from the front of the packet list and applies some action to it.
 * @param action Action to be applied. It should define
 *               "bool operator()(CargoPacket *)". If true is returned the
 *               cargo packet will be removed from the list. Otherwise it
 *               will be kept and the loop will be aborted.
 */
template<class Taction>
void VehicleCargoList::ShiftCargo(Taction action)
{
	Iterator it(this->packets.begin());
	while (it != this->packets.end() && action.MaxMove() > 0) {
		CargoPacket *cp = *it;
		if (action(cp)) {
			it = this->packets.erase(it);
		} else {
			break;
		}
	}
}

/**
 * Pops cargo from the back of the packet list and applies some action to it.
 * @param action Action to be applied. It should define
 *               "bool operator()(CargoPacket *)". If true is returned the
 *               cargo packet will be removed from the list. Otherwise it
 *               will be kept and the loop will be aborted.
 */
template<class Taction>
void VehicleCargoList::PopCargo(Taction action)
{
	ReverseIterator it(this->packets.rbegin());
	while (it != this->packets.rend() && action.MaxMove() > 0) {
		CargoPacket *cp = *it;
		if (action(cp)) {
			this->packets.erase((++it).base());
		} else {
			break;
		}
	}
}

/**
 * Invalidates the cached data and rebuilds it.
 */
template <class Tinst, class Tcont>
void CargoList<Tinst, Tcont>::InvalidateCache()
{
	this->count = 0;
	this->cargo_days_in_transit = 0;

	for (ConstIterator it(this->packets.begin()); it != this->packets.end(); it++) {
		static_cast<Tinst *>(this)->AddToCache(*it);
	}
}

/**
 * Update the cached values to reflect the removal of this packet or part of it.
 * Decreases count, feeder share and days_in_transit.
 * @param cp Packet to be removed from cache.
 * @param count Amount of cargo from the given packet to be removed.
 */
void VehicleCargoList::RemoveFromCache(const CargoPacket *cp, uint count)
{
	this->feeder_share -= cp->feeder_share;
	this->Parent::RemoveFromCache(cp, count);
}

/**
 * Update the cache to reflect adding of this packet.
 * Increases count, feeder share and days_in_transit.
 * @param cp New packet to be inserted.
 */
void VehicleCargoList::AddToCache(const CargoPacket *cp)
{
	this->feeder_share += cp->feeder_share;
	this->Parent::AddToCache(cp);
}

/**
 * Removes a packet or part of it from the metadata.
 * @param cp Packet to be removed.
 * @param action MoveToAction of the packet (for updating the counts).
 * @param count Amount of cargo to be removed.
 */
void VehicleCargoList::RemoveFromMeta(const CargoPacket *cp, Action action, uint count)
{
	this->AssertCountConsistence();
	this->RemoveFromCache(cp, count);
	this->action_counts[action] -= count;
	this->AssertCountConsistence();
}

/**
 * Adds a packet to the metadata.
 * @param cp Packet to be added.
 * @param action MoveToAction of the packet.
 */
void VehicleCargoList::AddToMeta(const CargoPacket *cp, Action action)
{
	this->AssertCountConsistence();
	this->AddToCache(cp);
	this->action_counts[action] += cp->count;
	this->AssertCountConsistence();
}

/**
 * Ages the all cargo in this list.
 */
void VehicleCargoList::AgeCargo()
{
	for (ConstIterator it(this->packets.begin()); it != this->packets.end(); it++) {
		CargoPacket *cp = *it;
		/* If we're at the maximum, then we can't increase no more. */
		if (cp->days_in_transit == 0xFF) continue;

		cp->days_in_transit++;
		this->cargo_days_in_transit += cp->count;
	}
}

/**
 * Stages cargo for unloading. The cargo is sorted so that packets to be
 * transferred, delivered or kept are in consecutive chunks in the list. At the
 * same time the designation_counts are updated to reflect the size of those
 * chunks.
 * @param accepted If the cargo will be accepted at the station.
 * @param current_station ID of the station.
 * @param order_flags OrderUnloadFlags that will apply to the unload operation.
 * return If any cargo will be unloaded.
 */
bool VehicleCargoList::Stage(bool accepted, StationID current_station, uint8 order_flags, const GoodsEntry *ge, CargoPayment *payment)
{
	this->AssertCountConsistence();
	assert(this->action_counts[A_LOAD] == 0);
	this->action_counts[A_TRANSFER] = this->action_counts[A_DELIVER] = this->action_counts[A_KEEP] = 0;
	Iterator deliver = this->packets.end();
	Iterator it = this->packets.begin();
	uint sum = 0;
	while (sum < this->count) {
		CargoPacket *cp = *it;
		this->packets.erase(it++);
		if ((order_flags & OUFB_TRANSFER) != 0 || (!accepted && (order_flags & OUFB_UNLOAD) != 0)) {
			this->packets.push_front(cp);
			this->action_counts[A_TRANSFER] += cp->count;
		} else if (accepted && current_station != cp->source && (order_flags & OUFB_NO_UNLOAD) == 0) {
			this->packets.insert(deliver, cp);
			this->action_counts[A_DELIVER] += cp->count;
		} else {
			this->packets.push_back(cp);
			if (deliver == this->packets.end()) --deliver;
			this->action_counts[A_KEEP] += cp->count;
		}
		sum += cp->count;
	}
	this->AssertCountConsistence();
	return this->action_counts[A_DELIVER] > 0 || this->action_counts[A_TRANSFER] > 0;
}

/*
 *
 * Station cargo list implementation
 *
 */

/**
 * Appends the given cargo packet to the range of packets with the same next station
 * @warning After appending this packet may not exist anymore!
 * @note Do not use the cargo packet anymore after it has been appended to this CargoList!
 * @param next the next hop
 * @param cp the cargo packet to add
 * @pre cp != NULL
 */
void StationCargoList::Append(CargoPacket *cp, StationID next)
{
	assert(cp != NULL);
	this->AddToCache(cp);

	StationCargoPacketMap::List &list = this->packets[next];
	for (StationCargoPacketMap::List::reverse_iterator it(list.rbegin()); it != list.rend(); it++) {
		CargoPacket *icp = *it;
		if (StationCargoList::AreMergable(icp, cp) && icp->count + cp->count <= CargoPacket::MAX_COUNT) {
			icp->Merge(cp);
			return;
		}
	}

	/* The packet could not be merged with another one */
	list.push_back(cp);
}

/**
 * Route all packets with station "to" as next hop to a different place.
 * @param to Station to exclude from routing.
 * @oaram good GoodsEntry to get the routing info from.
 */
void StationCargoList::RerouteStalePackets(StationID to, const GoodsEntry *good)
{
	std::pair<Iterator, Iterator> range(this->packets.equal_range(to));
	for (Iterator it(range.first); it != range.second && it.GetKey() == to;) {
		CargoPacket *packet = *it;
		it = this->packets.erase(it);

		StationID next = good->GetVia(packet->source, to);
		assert(next != to);

		/* Legal, as insert doesn't invalidate iterators in the MultiMap, however
		 * this might insert the packet between range.first and range.second (which might be end())
		 * This is why we check for GetKey above to avoid infinite loops. */
		this->packets.Insert(next, packet);
	}
}

/**
 * Truncate where each destination loses roughly the same percentage of its cargo.
 * This is done by randomizing the selection of packets to be removed.
 * @param max_remaining maximum amount of cargo to keep in the list.
 */
void StationCargoList::RandomTruncate(uint max_remaining)
{
	uint prev_count = this->count;
	while (this->count > max_remaining) {
		for (Iterator it(this->packets.begin()); it != this->packets.end();) {
			if (RandomRange(prev_count) < max_remaining) {
				++it;
				continue;
			}
			CargoPacket *packet = *it;
			uint diff = this->count - max_remaining;
			if (packet->count > diff) {
				packet->count -= diff;
				this->count = max_remaining;
				this->cargo_days_in_transit -= packet->days_in_transit * diff;
				return;
			} else {
				it = this->packets.erase(it);
				this->RemoveFromCache(packet, packet->count);
				delete packet;
			}
		}
	}
}

/** Invalidates the cached data and rebuild it. */
void VehicleCargoList::InvalidateCache()
{
	this->feeder_share = 0;
	this->Parent::InvalidateCache();
}

/**
 * Decides if a packet needs to be split.
 * @param cp Packet to be either split or moved in one piece.
 * @return Either new packet if splitting was necessary or the given one
 *         otherwise.
 */
template<class Tsource, class Tdest>
CargoPacket *CargoMovement<Tsource, Tdest>::Preprocess(CargoPacket *cp)
{
	if (this->max_move < cp->Count()) {
		cp = cp->Split(this->max_move);
		this->max_move = 0;
	} else {
		this->max_move -= cp->Count();
	}
	return cp;
}

/**
 * Delivers some cargo.
 * @param cp Packet to be delivered.
 * @return True if the packet was completely delivered, false if only part of
 *         it was.
 */
bool CargoDelivery::operator()(CargoPacket *cp)
{
	if (this->max_move >= cp->Count()) {
		this->payment->PayFinalDelivery(cp, cp->Count());
		this->max_move -= cp->Count();
		this->source->RemoveFromMeta(cp, VehicleCargoList::A_DELIVER, cp->Count());
		delete cp;
		return true;
	} else {
		this->payment->PayFinalDelivery(cp, this->max_move);
		this->source->RemoveFromMeta(cp, VehicleCargoList::A_DELIVER, this->max_move);
		cp->Reduce(this->max_move);
		this->max_move = 0;
		return false;
	}
}

/**
 * Returns some reserved cargo.
 * @param cp Packet to be returned.
 * @return True if the packet was completely returned, false if part of it was.
 */
bool CargoReturn::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) cp_new = cp;
	this->source->RemoveFromMeta(cp_new, VehicleCargoList::A_LOAD, cp_new->Count());
	this->destination->Return(cp_new, this->next_station);
	return cp_new == cp;
}

/**
 * Loads some cargo onto a vehicle.
 * @param cp Packet to be loaded.
 * @return True if the packet was completely loaded, false if part of it was.
 */
bool CargoLoad::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) return false;
	cp_new->SetLoadPlace(this->load_place);
	this->source->Load(cp_new);
	this->destination->Append(cp_new, VehicleCargoList::A_KEEP);
	return cp_new == cp;
}

/**
 * Reserves some cargo for loading.
 * @param cp Packet to be reserved.
 * @return True if the packet was completely reserved, false if part of it was.
 */
bool CargoReservation::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) return false;
	cp_new->SetLoadPlace(this->load_place);
	this->source->Reserve(cp_new);
	this->destination->Append(cp_new, VehicleCargoList::A_LOAD);
	return cp_new == cp;
}

/**
 * Transfers some cargo from a vehicle to a station.
 * @param cp Packet to be transfered.
 * @return True if the packet was completely reserved, false if part of it was.
 */
bool CargoTransfer::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) return false;
	this->source->RemoveFromMeta(cp_new, VehicleCargoList::A_TRANSFER, cp_new->Count());
	//TODO: do this in Stage()
	//cp_new->AddFeederShare(payment->PayTransfer(cp_new, cp_new->Count()));
	this->destination->Append(cp_new, cp_new->NextStation());
	return cp_new == cp;
}

/**
 * Shifts some cargo from a vehicle to another one.
 * @param cp Packet to be shifted.
 * @return True if the packet was completely shifted, false if part of it was.
 */
bool CargoShift::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) cp_new = cp;
	this->source->RemoveFromMeta(cp_new, VehicleCargoList::A_KEEP, cp_new->Count());
	this->destination->Append(cp_new, VehicleCargoList::A_KEEP);
	return cp_new == cp;
}

/*
 * We have to instantiate everything we want to be usable.
 */
template class CargoList<VehicleCargoList, CargoPacketList>;
template class CargoList<StationCargoList, StationCargoPacketMap>;
