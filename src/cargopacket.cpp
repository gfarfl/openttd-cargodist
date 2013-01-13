/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file cargopacket.cpp Implementation of the cargo packets. */

#include "stdafx.h"
#include "core/pool_func.hpp"
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
template <class Tinst>
CargoList<Tinst>::~CargoList()
{
	for (Iterator it(this->packets.begin()); it != this->packets.end(); ++it) {
		delete *it;
	}
}

/**
 * Empty the cargo list, but don't free the cargo packets;
 * the cargo packets are cleaned by CargoPacket's CleanPool.
 */
template <class Tinst>
void CargoList<Tinst>::OnCleanPool()
{
	this->packets.clear();
}

/**
 * Update the cached values to reflect the removal of this packet or part of it.
 * Decreases count and days_in_transit.
 * @param cp Packet to be removed from cache.
 * @param count Amount of cargo from the given packet to be removed.
 */
template <class Tinst>
void CargoList<Tinst>::RemoveFromCache(const CargoPacket *cp, uint count)
{
	this->count                 -= count;
	this->cargo_days_in_transit -= cp->days_in_transit * count;
}

/**
 * Update the cache to reflect adding of this packet.
 * Increases count and days_in_transit.
 * @param cp New packet to be inserted.
 */
template <class Tinst>
void CargoList<Tinst>::AddToCache(const CargoPacket *cp)
{
	this->count                 += cp->count;
	this->cargo_days_in_transit += cp->days_in_transit * cp->count;
}

/**
 * Appends the given cargo packet. Tries to merge it with another one in the
 * packets list. If no fitting packet is found, appends it.
 * @warning After appending this packet may not exist anymore!
 * @note Do not use the cargo packet anymore after it has been appended to this CargoList!
 * @param cp Cargo packet to add.
 * @pre cp != NULL
 */
template <class Tinst>
void CargoList<Tinst>::Append(CargoPacket *cp)
{
	assert(cp != NULL);
	static_cast<Tinst *>(this)->AddToCache(cp);

	for (List::reverse_iterator it(this->packets.rbegin()); it != this->packets.rend(); it++) {
		CargoPacket *icp = *it;
		if (CargoList<Tinst>::TryMerge(icp, cp)) return;
	}

	/* The packet could not be merged with another one */
	this->packets.push_back(cp);
}

/**
 * Truncates the cargo in this list to the given amount. It leaves the
 * first count cargo entities and removes the rest.
 * @param max_remaining Maximum amount of entities to be in the list after the command.
 */
template <class Tinst>
void CargoList<Tinst>::Truncate(uint max_remaining)
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

template <class Tinst>
/* static */ bool CargoList<Tinst>::TryMerge(CargoPacket *icp, CargoPacket *cp)
{
	if (Tinst::AreMergable(icp, cp) &&
			icp->count + cp->count <= CargoPacket::MAX_COUNT) {
		icp->Merge(cp);
		return true;
	} else {
		return false;
	}
}

// TODO: We cannot allow appends in the middle of the list as Reassign may shift the counts.
// Append is only allowed at the beginning or the end of the list. The counts are checked
// for 0 to find out if allowed. If reserved, no searching.
void VehicleCargoList::Append(CargoPacket *cp, Designation mode)
{
	assert(cp != NULL);
	this->AddToMeta(cp, mode);

	if (this->count == cp->count) {
		this->packets.push_back(cp);
		return;
	}

	bool reverse = true;
	if (mode == D_BEGIN) {
		reverse = false;
	} else if (mode != D_END - 1) {
		for (int i = D_END; i != D_BEGIN;) {
			--i;
			if (reverse) {
				if (mode == i) {
					break;
				} else if (this->designation_counts[i] > 0) {
					reverse = false;
				}
			} else {
				assert(mode >= i || this->designation_counts[i] == 0);
			}
		}
	}
	uint sum = cp->count;
	if (reverse) {
		for (ReverseIterator it(this->packets.rbegin()); it != this->packets.rend(); it++) {
			CargoPacket *icp = *it;
			if (VehicleCargoList::TryMerge(icp, cp)) return;
			sum += icp->count;
			if (sum >= this->designation_counts[mode]) {
				this->packets.push_back(cp);
				return;
			}
		}
	} else {
		for (Iterator it(this->packets.begin()); it != this->packets.end(); it++) {
			CargoPacket *icp = *it;
			if (VehicleCargoList::TryMerge(icp, cp)) return;
			sum += icp->count;
			if (sum >= this->designation_counts[mode]) {
				this->packets.push_front(cp);
				return;
			}
		}
	}

	NOT_REACHED();
}

/**
 * Returns all reserved cargo to the station and removes it from the cache.
 * @param dest Station the cargo is returned to.
 * @param count Maximum amount of cargo to move;
 */
uint VehicleCargoList::Return(StationCargoList *dest, uint max_move)
{
	max_move = min(this->designation_counts[D_LOAD], max_move);
	this->PopCargo(CargoReturn(this, dest, max_move));
	return max_move;
}

/**
 * Load packets from the reservation list.
 * @param dest Vehicle cargo list where the cargo resides.
 * @param count Number of cargo to load.
 * @return Amount of cargo actually loaded.
 */
uint StationCargoList::Load(VehicleCargoList *dest, uint max_move, TileIndex load_place)
{
	uint move = min(dest->DesignationCount(VehicleCargoList::D_LOAD), max_move);
	if (move > 0) {
		this->reserved_count -= move;
		dest->Reassign(move, VehicleCargoList::D_LOAD, VehicleCargoList::D_KEEP);
	} else {
		move = min(this->count, max_move);
		this->ShiftCargo(CargoLoad(this, dest, move, load_place));
	}
	return move;
}

uint StationCargoList::Reserve(VehicleCargoList *dest, uint max_move, TileIndex load_place)
{
	max_move = min(this->count, max_move);
	this->ShiftCargo(CargoReservation(this, dest, max_move, load_place));
	return max_move;
}

uint VehicleCargoList::Unload(StationCargoList *dest, uint max_move, CargoPayment *payment)
{
	uint moved = 0;
	if (this->designation_counts[D_TRANSFER] > 0) {
		uint move = min(this->designation_counts[D_TRANSFER], max_move);
		this->ShiftCargo(CargoTransfer(this, dest, move, payment));
		moved += move;
	}
	if (this->designation_counts[D_TRANSFER] == 0 && this->designation_counts[D_DELIVER] > 0 && moved < max_move) {
		uint move = min(this->designation_counts[D_DELIVER], max_move - moved);
		this->ShiftCargo(CargoDelivery(this, move, payment));
		moved += move;
	}
	return moved;
}

uint VehicleCargoList::Shift(VehicleCargoList *other, uint max_move)
{
	max_move = max(this->count, max_move);
	this->PopCargo(CargoShift(this, other, max_move));
	return max_move;
}

/**
 * Moves the given amount of cargo to another list.
 * Depending on the value of mta the side effects of this function differ:
 *  - MTA_FINAL_DELIVERY: Destroys the packets that do not originate from a specific station.
 *  - MTA_CARGO_LOAD:     Sets the loaded_at_xy value of the moved packets.
 *  - MTA_TRANSFER:       Just move without side effects.
 *  - MTA_UNLOAD:         Just move without side effects.
 * @param dest  Destination to move the cargo to.
 * @param max_move Amount of cargo entities to move.
 * @param mta   How to handle the moving (side effects).
 * @param data  Depending on mta the data of this variable differs:
 *              - MTA_FINAL_DELIVERY - Station ID of packet's origin not to remove.
 *              - MTA_CARGO_LOAD     - Station's tile index of load.
 *              - MTA_TRANSFER       - Unused.
 *              - MTA_UNLOAD         - Unused.
 * @param payment The payment helper.
 *
 * @pre mta == MTA_FINAL_DELIVERY || dest != NULL
 * @pre mta == MTA_UNLOAD || mta == MTA_CARGO_LOAD || payment != NULL
 * @return True if there are still packets that might be moved from this cargo list.
 */
template <class Tinst>
template <class Taction>
void CargoList<Tinst>::ShiftCargo(Taction action)
{
	Iterator it(this->packets.begin());
	while (it != this->packets.end() && action.MaxMove() > 0) {
		CargoPacket *cp = *it;
		if (!action(cp)) {
			it = this->packets.erase(it);
		} else {
			break;
		}
	}
}

template <class Tinst>
template <class Taction>
void CargoList<Tinst>::PopCargo(Taction action)
{
	ReverseIterator it(this->packets.rbegin());
	while (it != this->packets.rend() && action.MaxMove() > 0) {
		CargoPacket *cp = *it;
		if (!action(cp)) {
			this->packets.erase((++it).base());
		} else {
			break;
		}
	}
}

/** Invalidates the cached data and rebuilds it. */
template <class Tinst>
void CargoList<Tinst>::InvalidateCache()
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

void VehicleCargoList::RemoveFromMeta(const CargoPacket *cp, Designation mode, uint count)
{
	this->AssertCountConsistence();
	this->RemoveFromCache(cp, count);
	this->designation_counts[mode] -= count;
	this->AssertCountConsistence();
}

void VehicleCargoList::AddToMeta(const CargoPacket *cp, Designation mode)
{
	this->AssertCountConsistence();
	this->AddToCache(cp);
	this->designation_counts[mode] += cp->count;
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

bool VehicleCargoList::Stage(bool accepted, StationID current_station, uint8 order_flags)
{
	this->AssertCountConsistence();
	assert(this->designation_counts[D_LOAD] == 0);
	this->designation_counts[D_TRANSFER] = this->designation_counts[D_DELIVER] = this->designation_counts[D_KEEP] = 0;
	Iterator deliver = this->packets.end();
	Iterator it = this->packets.begin();
	uint sum = 0;
	while (sum < this->count) {
		CargoPacket *cp = *it;
		this->packets.erase(it++);
		if (accepted && current_station != cp->source && (order_flags & OUFB_NO_UNLOAD) == 0) {
			this->packets.insert(deliver, cp);
			this->designation_counts[D_DELIVER] += cp->count;
		} else if ((order_flags & OUFB_TRANSFER) != 0 || (!accepted && (order_flags & OUFB_UNLOAD) != 0)) {
			this->packets.push_front(cp);
			this->designation_counts[D_TRANSFER] += cp->count;
		} else {
			this->packets.push_back(cp);
			if (deliver == this->packets.end()) --deliver;
			this->designation_counts[D_KEEP] += cp->count;
		}
		sum += cp->count;
	}
	this->AssertCountConsistence();
	return this->designation_counts[D_DELIVER] > 0 || this->designation_counts[D_TRANSFER] > 0;
}

/**
 * Balance the reserved lists by transferring transferring some packets.
 * @param other Cargo list to balance with.
 * @param share Share of the overall reserved cargo each of the vehicles should get.
 * @param max_move Maximum amount of cargo to be moved.
 */
uint VehicleCargoList::Balance(VehicleCargoList *other, uint max_move, uint share)
{
	/* Move at most as much cargo as needed so that both have the same amount. */
	max_move = min((this->designation_counts[D_LOAD] - other->designation_counts[D_LOAD]) / 2, max_move);
	/* Move at least as much cargo to fill the other vehicle to its share (if possible). */
	uint min_move = min(share - other->designation_counts[D_LOAD], max_move);
	uint moved = 0;
	ReverseIterator it(this->packets.rbegin());
	while (it != this->packets.rend() && moved < min_move) {
		CargoPacket *cp = *it;
		if (cp->count <= max_move - moved) {
			this->RemoveFromMeta(cp, D_LOAD, cp->count);
			this->packets.erase((++it).base());
			other->Append(cp, D_LOAD);
			moved += cp->count;
		} else if (CargoPacket::CanAllocateItem()) {
			uint move = (max_move + min_move) / 2 - moved;
			cp->count -= move;
			CargoPacket *cp_new = new CargoPacket(move, cp->days_in_transit, cp->source, cp->source_xy, cp->loaded_at_xy, 0, cp->source_type, cp->source_id);
			this->RemoveFromMeta(cp_new, D_LOAD, cp_new->count);
			other->Append(cp_new, D_LOAD);
			moved += move;
		} else {
			/* If this actually happens it will desync. There is not much we
			 * can do when out of memory, though. */
			break;
		}
	}
	return moved;
}

/** Invalidates the cached data and rebuild it. */
void VehicleCargoList::InvalidateCache()
{
	this->feeder_share = 0;
	this->Parent::InvalidateCache();
}

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

bool CargoDelivery::operator()(CargoPacket *cp)
{
	if (this->max_move >= cp->Count()) {
		this->payment->PayFinalDelivery(cp, cp->Count());
		this->max_move -= cp->Count();
		this->source->RemoveFromMeta(cp, VehicleCargoList::D_DELIVER, cp->Count());
		delete cp;
		return false;
	} else {
		this->source->RemoveFromMeta(cp, VehicleCargoList::D_DELIVER, this->max_move);
		cp->Reduce(this->max_move);
		this->max_move = 0;
		return true;
	}
}

bool CargoReturn::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) cp_new = cp;
	this->source->RemoveFromMeta(cp_new, VehicleCargoList::D_LOAD, cp_new->Count());
	this->destination->Return(cp_new);
	return cp_new != cp;
}

bool CargoLoad::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) return true;
	cp_new->SetLoadPlace(this->load_place);
	this->source->Load(cp_new);
	this->destination->Append(cp_new, VehicleCargoList::D_KEEP);
	return cp_new != cp;
}

bool CargoReservation::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) return true;
	cp_new->SetLoadPlace(this->load_place);
	this->source->Reserve(cp_new);
	this->destination->Append(cp_new, VehicleCargoList::D_LOAD);
	return cp_new != cp;
}

bool CargoTransfer::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) return true;
	this->source->RemoveFromMeta(cp_new, VehicleCargoList::D_TRANSFER, cp_new->Count());
	cp_new->AddFeederShare(payment->PayTransfer(cp_new, cp_new->Count()));
	this->destination->Append(cp_new);
	return cp_new != cp;
}

bool CargoShift::operator()(CargoPacket *cp)
{
	CargoPacket *cp_new = this->Preprocess(cp);
	if (cp_new == NULL) cp_new = cp;
	this->source->RemoveFromMeta(cp_new, VehicleCargoList::D_KEEP, cp_new->Count());
	this->destination->Append(cp_new, VehicleCargoList::D_KEEP);
	return cp_new != cp;
}

/*
 * We have to instantiate everything we want to be usable.
 */
template class CargoList<VehicleCargoList>;
template class CargoList<StationCargoList>;
