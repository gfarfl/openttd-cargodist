/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file articulated_vehicles.cpp Implementation of articulated vehicles. */

#include "stdafx.h"
#include "train.h"
#include "roadveh.h"
#include "newgrf_engine.h"
#include "vehicle_func.h"

#include "table/strings.h"
#include "table/sprites.h"

static const uint MAX_ARTICULATED_PARTS = 100; ///< Maximum of articulated parts per vehicle, i.e. when to abort calling the articulated vehicle callback.

uint CountArticulatedParts(EngineID engine_type, bool purchase_window)
{
	if (!HasBit(EngInfo(engine_type)->callbackmask, CBM_VEHICLE_ARTIC_ENGINE)) return 0;

	/* If we can't allocate a vehicle now, we can't allocate it in the command
	 * either, so it doesn't matter how many articulated parts there are. */
	if (!Vehicle::CanAllocateItem()) return 0;

	Vehicle *v = NULL;
	if (!purchase_window) {
		v = new Vehicle();
		v->engine_type = engine_type;
	}

	uint i;
	for (i = 1; i < MAX_ARTICULATED_PARTS; i++) {
		uint16 callback = GetVehicleCallback(CBID_VEHICLE_ARTIC_ENGINE, i, 0, engine_type, v);
		if (callback == CALLBACK_FAILED || GB(callback, 0, 8) == 0xFF) break;
	}

	delete v;

	return i - 1;
}


/**
 * Returns the default (non-refitted) capacity of a specific EngineID.
 * @param engine the EngineID of iterest
 * @param type the type of the engine
 * @param cargo_type returns the default cargo type, if needed
 * @return capacity
 */
static inline uint16 GetVehicleDefaultCapacity(EngineID engine, VehicleType type, CargoID *cargo_type)
{
	const Engine *e = Engine::Get(engine);
	CargoID cargo = (e->CanCarryCargo() ? e->GetDefaultCargoType() : (CargoID)CT_INVALID);
	if (cargo_type != NULL) *cargo_type = cargo;
	if (cargo == CT_INVALID) return 0;
	return e->GetDisplayDefaultCapacity();
}

/**
 * Returns all cargos a vehicle can carry.
 * @param engine the EngineID of iterest
 * @param type the type of the engine
 * @param include_initial_cargo_type if true the default cargo type of the vehicle is included; if false only the refit_mask
 * @return bit set of CargoIDs
 */
static inline uint32 GetAvailableVehicleCargoTypes(EngineID engine, VehicleType type, bool include_initial_cargo_type)
{
	uint32 cargos = 0;
	CargoID initial_cargo_type;

	if (GetVehicleDefaultCapacity(engine, type, &initial_cargo_type) > 0) {
		if (type != VEH_SHIP || ShipVehInfo(engine)->refittable) {
			const EngineInfo *ei = EngInfo(engine);
			cargos = ei->refit_mask;
		}
		if (include_initial_cargo_type && initial_cargo_type < NUM_CARGO) SetBit(cargos, initial_cargo_type);
	}

	return cargos;
}

CargoArray GetCapacityOfArticulatedParts(EngineID engine, VehicleType type)
{
	CargoArray capacity;

	CargoID cargo_type;
	uint16 cargo_capacity = GetVehicleDefaultCapacity(engine, type, &cargo_type);
	if (cargo_type < NUM_CARGO) capacity[cargo_type] = cargo_capacity;

	if (type != VEH_TRAIN && type != VEH_ROAD) return capacity;

	if (!HasBit(EngInfo(engine)->callbackmask, CBM_VEHICLE_ARTIC_ENGINE)) return capacity;

	for (uint i = 1; i < MAX_ARTICULATED_PARTS; i++) {
		uint16 callback = GetVehicleCallback(CBID_VEHICLE_ARTIC_ENGINE, i, 0, engine, NULL);
		if (callback == CALLBACK_FAILED || GB(callback, 0, 8) == 0xFF) break;

		EngineID artic_engine = GetNewEngineID(GetEngineGRF(engine), type, GB(callback, 0, 7));

		cargo_capacity = GetVehicleDefaultCapacity(artic_engine, type, &cargo_type);
		if (cargo_type < NUM_CARGO) capacity[cargo_type] += cargo_capacity;
	}

	return capacity;
}

/**
 * Checks whether any of the articulated parts is refittable
 * @param engine the first part
 * @return true if refittable
 */
bool IsArticulatedVehicleRefittable(EngineID engine)
{
	if (IsEngineRefittable(engine)) return true;

	const Engine *e = Engine::Get(engine);
	if (e->type != VEH_TRAIN && e->type != VEH_ROAD) return false;

	if (!HasBit(e->info.callbackmask, CBM_VEHICLE_ARTIC_ENGINE)) return false;

	for (uint i = 1; i < MAX_ARTICULATED_PARTS; i++) {
		uint16 callback = GetVehicleCallback(CBID_VEHICLE_ARTIC_ENGINE, i, 0, engine, NULL);
		if (callback == CALLBACK_FAILED || GB(callback, 0, 8) == 0xFF) break;

		EngineID artic_engine = GetNewEngineID(GetEngineGRF(engine), e->type, GB(callback, 0, 7));
		if (IsEngineRefittable(artic_engine)) return true;
	}

	return false;
}

/**
 * Ors the refit_masks of all articulated parts.
 * @param engine the first part
 * @param type the vehicle type
 * @param include_initial_cargo_type if true the default cargo type of the vehicle is included; if false only the refit_mask
 * @return bit mask of CargoIDs which are a refit option for at least one articulated part
 */
uint32 GetUnionOfArticulatedRefitMasks(EngineID engine, VehicleType type, bool include_initial_cargo_type)
{
	uint32 cargos = GetAvailableVehicleCargoTypes(engine, type, include_initial_cargo_type);

	if (type != VEH_TRAIN && type != VEH_ROAD) return cargos;

	if (!HasBit(EngInfo(engine)->callbackmask, CBM_VEHICLE_ARTIC_ENGINE)) return cargos;

	for (uint i = 1; i < MAX_ARTICULATED_PARTS; i++) {
		uint16 callback = GetVehicleCallback(CBID_VEHICLE_ARTIC_ENGINE, i, 0, engine, NULL);
		if (callback == CALLBACK_FAILED || GB(callback, 0, 8) == 0xFF) break;

		EngineID artic_engine = GetNewEngineID(GetEngineGRF(engine), type, GB(callback, 0, 7));
		cargos |= GetAvailableVehicleCargoTypes(artic_engine, type, include_initial_cargo_type);
	}

	return cargos;
}

/**
 * Ands the refit_masks of all articulated parts.
 * @param engine the first part
 * @param type the vehicle type
 * @param include_initial_cargo_type if true the default cargo type of the vehicle is included; if false only the refit_mask
 * @return bit mask of CargoIDs which are a refit option for every articulated part (with default capacity > 0)
 */
uint32 GetIntersectionOfArticulatedRefitMasks(EngineID engine, VehicleType type, bool include_initial_cargo_type)
{
	uint32 cargos = UINT32_MAX;

	uint32 veh_cargos = GetAvailableVehicleCargoTypes(engine, type, include_initial_cargo_type);
	if (veh_cargos != 0) cargos &= veh_cargos;

	if (type != VEH_TRAIN && type != VEH_ROAD) return cargos;

	if (!HasBit(EngInfo(engine)->callbackmask, CBM_VEHICLE_ARTIC_ENGINE)) return cargos;

	for (uint i = 1; i < MAX_ARTICULATED_PARTS; i++) {
		uint16 callback = GetVehicleCallback(CBID_VEHICLE_ARTIC_ENGINE, i, 0, engine, NULL);
		if (callback == CALLBACK_FAILED || GB(callback, 0, 8) == 0xFF) break;

		EngineID artic_engine = GetNewEngineID(GetEngineGRF(engine), type, GB(callback, 0, 7));
		veh_cargos = GetAvailableVehicleCargoTypes(artic_engine, type, include_initial_cargo_type);
		if (veh_cargos != 0) cargos &= veh_cargos;
	}

	return cargos;
}


/**
 * Tests if all parts of an articulated vehicle are refitted to the same cargo.
 * Note: Vehicles not carrying anything are ignored
 * @param v the first vehicle in the chain
 * @param cargo_type returns the common CargoID if needed. (CT_INVALID if no part is carrying something or they are carrying different things)
 * @return true if some parts are carrying different cargos, false if all parts are carrying the same (nothing is also the same)
 */
bool IsArticulatedVehicleCarryingDifferentCargos(const Vehicle *v, CargoID *cargo_type)
{
	CargoID first_cargo = CT_INVALID;

	do {
		if (v->cargo_cap > 0 && v->cargo_type != CT_INVALID) {
			if (first_cargo == CT_INVALID) first_cargo = v->cargo_type;
			if (first_cargo != v->cargo_type) {
				if (cargo_type != NULL) *cargo_type = CT_INVALID;
				return true;
			}
		}

		switch (v->type) {
			case VEH_TRAIN:
				v = Train::From(v)->HasArticulatedPart() ? Train::From(v)->GetNextArticPart() : NULL;
				break;

			case VEH_ROAD:
				v = RoadVehicle::From(v)->HasArticulatedPart() ? v->Next() : NULL;
				break;

			default:
				v = NULL;
				break;
		}
	} while (v != NULL);

	if (cargo_type != NULL) *cargo_type = first_cargo;
	return false;
}

/**
 * Checks whether the specs of freshly build articulated vehicles are consistent with the information specified in the purchase list.
 * Only essential information is checked to leave room for magic tricks/workarounds to grfcoders.
 * It checks:
 *   For autoreplace/-renew:
 *    - Default cargo type (without capacity)
 *    - intersection and union of refit masks.
 */
void CheckConsistencyOfArticulatedVehicle(const Vehicle *v)
{
	const Engine *engine = Engine::Get(v->engine_type);

	uint32 purchase_refit_union = GetUnionOfArticulatedRefitMasks(v->engine_type, v->type, true);
	uint32 purchase_refit_intersection = GetIntersectionOfArticulatedRefitMasks(v->engine_type, v->type, true);
	CargoArray purchase_default_capacity = GetCapacityOfArticulatedParts(v->engine_type, v->type);

	uint32 real_refit_union = 0;
	uint32 real_refit_intersection = UINT_MAX;
	CargoArray real_default_capacity;

	do {
		uint32 refit_mask = GetAvailableVehicleCargoTypes(v->engine_type, v->type, true);
		real_refit_union |= refit_mask;
		if (refit_mask != 0) real_refit_intersection &= refit_mask;

		assert(v->cargo_type < NUM_CARGO);
		real_default_capacity[v->cargo_type] += v->cargo_cap;

		switch (v->type) {
			case VEH_TRAIN:
				v = Train::From(v)->HasArticulatedPart() ? Train::From(v)->GetNextArticPart() : NULL;
				break;

			case VEH_ROAD:
				v = RoadVehicle::From(v)->HasArticulatedPart() ? v->Next() : NULL;
				break;

			default:
				v = NULL;
				break;
		}
	} while (v != NULL);

	/* Check whether the vehicle carries more cargos than expected */
	bool carries_more = false;
	for (CargoID cid = 0; cid < NUM_CARGO; cid++) {
		if (real_default_capacity[cid] != 0 && purchase_default_capacity[cid] == 0) {
			carries_more = true;
			break;
		}
	}

	/* show a warning once for each GRF after each game load */
	if (real_refit_union != purchase_refit_union || real_refit_intersection != purchase_refit_intersection || carries_more) {
		ShowNewGrfVehicleError(engine->index, STR_NEWGRF_BUGGY, STR_NEWGRF_BUGGY_ARTICULATED_CARGO, GBUG_VEH_REFIT, false);
	}
}

void AddArticulatedParts(Vehicle *first, VehicleType type)
{
	if (!HasBit(EngInfo(first->engine_type)->callbackmask, CBM_VEHICLE_ARTIC_ENGINE)) return;

	Vehicle *v = first;
	for (uint i = 1; i < MAX_ARTICULATED_PARTS; i++) {
		uint16 callback = GetVehicleCallback(CBID_VEHICLE_ARTIC_ENGINE, i, 0, first->engine_type, first);
		if (callback == CALLBACK_FAILED || GB(callback, 0, 8) == 0xFF) return;

		/* In the (very rare) case the GRF reported wrong number of articulated parts
		 * and we run out of available vehicles, bail out. */
		if (!Vehicle::CanAllocateItem()) return;

		EngineID engine_type = GetNewEngineID(GetEngineGRF(first->engine_type), type, GB(callback, 0, 7));
		bool flip_image = HasBit(callback, 7);

		const Engine *e_artic = Engine::Get(engine_type);
		switch (type) {
			default: NOT_REACHED();

			case VEH_TRAIN: {
				Train *front = Train::From(first);
				Train *t = new Train();
				v->SetNext(t);
				v = t;

				t->subtype = 0;
				t->track = front->track;
				t->railtype = front->railtype;
				t->tcache.first_engine = front->engine_type;

				t->spritenum = e_artic->u.rail.image_index;
				if (e_artic->CanCarryCargo()) {
					t->cargo_type = e_artic->GetDefaultCargoType();
					t->cargo_cap = e_artic->u.rail.capacity;  // Callback 36 is called when the consist is finished
				} else {
					t->cargo_type = front->cargo_type; // Needed for livery selection
					t->cargo_cap = 0;
				}

				t->SetArticulatedPart();
			} break;

			case VEH_ROAD: {
				RoadVehicle *front = RoadVehicle::From(first);
				RoadVehicle *rv = new RoadVehicle();
				v->SetNext(rv);
				v = rv;

				rv->subtype = 0;
				rv->rcache.first_engine = front->engine_type;
				rv->rcache.cached_veh_length = 8; // Callback is called when the consist is finished
				rv->state = RVSB_IN_DEPOT;

				rv->roadtype = front->roadtype;
				rv->compatible_roadtypes = front->compatible_roadtypes;

				rv->spritenum = e_artic->u.road.image_index;
				if (e_artic->CanCarryCargo()) {
					rv->cargo_type = e_artic->GetDefaultCargoType();
					rv->cargo_cap = e_artic->u.road.capacity;  // Callback 36 is called when the consist is finished
				} else {
					rv->cargo_type = front->cargo_type; // Needed for livery selection
					rv->cargo_cap = 0;
				}

				rv->SetArticulatedPart();
			} break;
		}

		/* get common values from first engine */
		v->direction = first->direction;
		v->owner = first->owner;
		v->tile = first->tile;
		v->x_pos = first->x_pos;
		v->y_pos = first->y_pos;
		v->z_pos = first->z_pos;
		v->build_year = first->build_year;
		v->vehstatus = first->vehstatus & ~VS_STOPPED;

		v->cargo_subtype = 0;
		v->max_speed = 0;
		v->max_age = 0;
		v->engine_type = engine_type;
		v->value = 0;
		v->cur_image = SPR_IMG_QUERY;
		v->random_bits = VehicleRandomBits();

		if (flip_image) v->spritenum++;

		VehicleMove(v, false);
	}
}
