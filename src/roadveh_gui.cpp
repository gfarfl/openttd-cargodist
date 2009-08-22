/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file roadveh_gui.cpp GUI for road vehicles. */

#include "stdafx.h"
#include "roadveh.h"
#include "window_gui.h"
#include "gfx_func.h"
#include "vehicle_gui.h"
#include "strings_func.h"
#include "vehicle_func.h"
#include "string_func.h"

#include "table/sprites.h"
#include "table/strings.h"

/**
 * Draw the details for the given vehicle at the given position
 *
 * @param v     current vehicle
 * @param left  The left most coordinate to draw
 * @param right The right most coordinate to draw
 * @param y     The y coordinate
 */
void DrawRoadVehDetails(const Vehicle *v, int left, int right, int y)
{
	const RoadVehicle *rv = RoadVehicle::From(v);

	uint y_offset = rv->HasArticulatedPart() ? 15 : 0;
	StringID str;
	Money feeder_share = 0;

	SetDParam(0, v->engine_type);
	SetDParam(1, v->build_year);
	SetDParam(2, v->value);
	DrawString(left, right, y + y_offset, STR_VEHICLE_INFO_BUILT_VALUE);

	if (rv->HasArticulatedPart()) {
		CargoArray max_cargo;
		StringID subtype_text[NUM_CARGO];
		char capacity[512];

		memset(subtype_text, 0, sizeof(subtype_text));

		for (const Vehicle *u = v; u != NULL; u = u->Next()) {
			max_cargo[u->cargo_type] += u->cargo_cap;
			if (u->cargo_cap > 0) {
				StringID text = GetCargoSubtypeText(u);
				if (text != STR_EMPTY) subtype_text[u->cargo_type] = text;
			}
		}

		GetString(capacity, STR_VEHICLE_DETAILS_TRAIN_ARTICULATED_RV_CAPACITY, lastof(capacity));

		bool first = true;
		for (CargoID i = 0; i < NUM_CARGO; i++) {
			if (max_cargo[i] > 0) {
				char buffer[128];

				SetDParam(0, i);
				SetDParam(1, max_cargo[i]);
				GetString(buffer, STR_JUST_CARGO, lastof(buffer));

				if (!first) strecat(capacity, ", ", lastof(capacity));
				strecat(capacity, buffer, lastof(capacity));

				if (subtype_text[i] != 0) {
					GetString(buffer, subtype_text[i], lastof(buffer));
					strecat(capacity, buffer, lastof(capacity));
				}

				first = false;
			}
		}

		DrawString(left, right, y + 10 + y_offset, capacity, TC_BLUE);

		for (const Vehicle *u = v; u != NULL; u = u->Next()) {
			if (u->cargo_cap == 0) continue;

			str = STR_VEHICLE_DETAILS_CARGO_EMPTY;
			if (!u->cargo.Empty()) {
				SetDParam(0, u->cargo_type);
				SetDParam(1, u->cargo.Count());
				SetDParam(2, u->cargo.Source());
				str = STR_VEHICLE_DETAILS_CARGO_FROM;
				feeder_share += u->cargo.FeederShare();
			}
			DrawString(left, right, y + 21 + y_offset, str);

			y_offset += 11;
		}

		y_offset -= 11;
	} else {
		SetDParam(0, v->cargo_type);
		SetDParam(1, v->cargo_cap);
		SetDParam(4, GetCargoSubtypeText(v));
		DrawString(left, right, y + 10 + y_offset, STR_VEHICLE_INFO_CAPACITY);

		str = STR_VEHICLE_DETAILS_CARGO_EMPTY;
		if (!v->cargo.Empty()) {
			SetDParam(0, v->cargo_type);
			SetDParam(1, v->cargo.Count());
			SetDParam(2, v->cargo.Source());
			str = STR_VEHICLE_DETAILS_CARGO_FROM;
			feeder_share += v->cargo.FeederShare();
		}
		DrawString(left, right, y + 21 + y_offset, str);
	}

	/* Draw Transfer credits text */
	SetDParam(0, feeder_share);
	DrawString(left, right, y + 33 + y_offset, STR_VEHICLE_INFO_FEEDER_CARGO_VALUE);
}

/**
 * Draws an image of a road vehicle chain
 * @param v Front vehicle
 + @param x x Position to start at
 * @param y y Position to draw at
 * @param seletion Selected vehicle to draw a border around
 * @param max_width Number of pixels space for drawing
 */
void DrawRoadVehImage(const Vehicle *v, int x, int y, VehicleID selection, int max_width)
{
	const RoadVehicle *u = RoadVehicle::From(v);
	int x_pos = 0;
	for (; u != NULL && x_pos < max_width; u = u->Next()) {
		Point offset;
		int width = u->GetDisplayImageWidth(&offset);

		SpriteID pal = (u->vehstatus & VS_CRASHED) ? PALETTE_CRASH : GetVehiclePalette(u);
		DrawSprite(u->GetImage(DIR_W), pal, x + x_pos + offset.x, y + 6 + offset.y);
		x_pos += width;
	}

	if (v->index == selection) {
		DrawFrameRect(x - 1, y - 1, x - 1 + x_pos, y + 12, COLOUR_WHITE, FR_BORDERONLY);
	}
}

void CcBuildRoadVeh(bool success, TileIndex tile, uint32 p1, uint32 p2)
{
	const Vehicle *v;

	if (!success) return;

	v = Vehicle::Get(_new_vehicle_id);
	if (v->tile == _backup_orders_tile) {
		_backup_orders_tile = 0;
		RestoreVehicleOrders(v);
	}
	ShowVehicleViewWindow(v);
}
