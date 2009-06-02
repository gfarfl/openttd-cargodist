/* $Id$ */

/** @file ship.h Base for ships. */

#ifndef SHIP_H
#define SHIP_H

#include "vehicle_base.h"
#include "engine_func.h"
#include "engine_base.h"
#include "economy_func.h"

void CcBuildShip(bool success, TileIndex tile, uint32 p1, uint32 p2);
void RecalcShipStuff(Vehicle *v);
void GetShipSpriteSize(EngineID engine, uint &width, uint &height);

/**
 * All ships have this type.
 */
struct Ship: public SpecializedVehicle<Ship, VEH_SHIP> {
	TrackBitsByte state;

	/** We don't want GCC to zero our struct! It already is zeroed and has an index! */
	Ship() : SpecializedVehicle<Ship, VEH_SHIP>() {}
	/** We want to 'destruct' the right class. */
	virtual ~Ship() { this->PreDestructor(); }

	const char *GetTypeString() const { return "ship"; }
	void MarkDirty();
	void UpdateDeltaXY(Direction direction);
	ExpensesType GetExpenseType(bool income) const { return income ? EXPENSES_SHIP_INC : EXPENSES_SHIP_RUN; }
	void PlayLeaveStationSound() const;
	bool IsPrimaryVehicle() const { return true; }
	SpriteID GetImage(Direction direction) const;
	int GetDisplaySpeed() const { return this->cur_speed / 2; }
	int GetDisplayMaxSpeed() const { return this->max_speed / 2; }
	Money GetRunningCost() const;
	bool IsInDepot() const { return this->state == TRACK_BIT_DEPOT; }
	bool Tick();
	void OnNewDay();
	Trackdir GetVehicleTrackdir() const;
	TileIndex GetOrderStationLocation(StationID station);
	bool FindClosestDepot(TileIndex *location, DestinationID *destination, bool *reverse);
};

#define FOR_ALL_SHIPS(var) FOR_ALL_VEHICLES_OF_TYPE(Ship, var)

#endif /* SHIP_H */
