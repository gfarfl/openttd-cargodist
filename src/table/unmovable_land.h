/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file unmovable_land.h Sprites to use and how to display them for unmovable tiles. */

static const DrawTileSeqStruct _draw_tile_transmitterlighthouse_data[] = {
	{   7,  7,  0,  2,  2, 70, {SPR_UNMOVABLE_TRANSMITTER, PAL_NONE}},
	{   4,  4,  0,  7,  7, 61, {SPR_UNMOVABLE_LIGHTHOUSE, PAL_NONE}},
};

#define TILE_SEQ_LINE(sz, img) { 0, 0, 0, 16, 16, sz, {img, PAL_NONE} },
#define TILE_SEQ_END() { (int8)0x80, 0, 0, 0, 0, 0, {0, 0} }

static const DrawTileSeqStruct _unmovable_display_nothing[] = {
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_8[] = {
	TILE_SEQ_LINE(20, SPR_MEDIUMHQ_NORTH_WALL | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_9[] = {
	TILE_SEQ_LINE(20, SPR_MEDIUMHQ_EAST_WALL  | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_10[] = {
	TILE_SEQ_LINE(20, SPR_MEDIUMHQ_WEST_WALL  | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_12[] = {
	TILE_SEQ_LINE(50, SPR_LARGEHQ_NORTH_BUILD | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_13[] = {
	TILE_SEQ_LINE(50, SPR_LARGEHQ_EAST_BUILD  | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_14[] = {
	TILE_SEQ_LINE(50, SPR_LARGEHQ_WEST_BUILD  | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_16[] = {
	TILE_SEQ_LINE(60, SPR_HUGEHQ_NORTH_BUILD  | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_17[] = {
	TILE_SEQ_LINE(60, SPR_HUGEHQ_EAST_BUILD   | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

static const DrawTileSeqStruct _unmovable_display_datas_18[] = {
	TILE_SEQ_LINE(60, SPR_HUGEHQ_WEST_BUILD   | (1 << PALETTE_MODIFIER_COLOUR))
	TILE_SEQ_END()
};

#undef TILE_SEQ_LINE
#undef TILE_SEQ_END

#define TILE_SPRITE_LINE(img, dtss) { {img | (1 << PALETTE_MODIFIER_COLOUR), PAL_NONE}, dtss },

static const DrawTileSprites _unmovable_display_datas[] = {
	TILE_SPRITE_LINE(SPR_TINYHQ_NORTH,         _unmovable_display_nothing)
	TILE_SPRITE_LINE(SPR_TINYHQ_EAST,          _unmovable_display_nothing)
	TILE_SPRITE_LINE(SPR_TINYHQ_WEST,          _unmovable_display_nothing)
	TILE_SPRITE_LINE(SPR_TINYHQ_SOUTH,         _unmovable_display_nothing)

	TILE_SPRITE_LINE(SPR_SMALLHQ_NORTH,        _unmovable_display_nothing)
	TILE_SPRITE_LINE(SPR_SMALLHQ_EAST,         _unmovable_display_nothing)
	TILE_SPRITE_LINE(SPR_SMALLHQ_WEST,         _unmovable_display_nothing)
	TILE_SPRITE_LINE(SPR_SMALLHQ_SOUTH,        _unmovable_display_nothing)

	TILE_SPRITE_LINE(SPR_MEDIUMHQ_NORTH,       _unmovable_display_datas_8)
	TILE_SPRITE_LINE(SPR_MEDIUMHQ_EAST,        _unmovable_display_datas_9)
	TILE_SPRITE_LINE(SPR_MEDIUMHQ_WEST,        _unmovable_display_datas_10)
	TILE_SPRITE_LINE(SPR_MEDIUMHQ_SOUTH,       _unmovable_display_nothing)

	TILE_SPRITE_LINE(SPR_LARGEHQ_NORTH_GROUND, _unmovable_display_datas_12)
	TILE_SPRITE_LINE(SPR_LARGEHQ_EAST_GROUND,  _unmovable_display_datas_13)
	TILE_SPRITE_LINE(SPR_LARGEHQ_WEST_GROUND,  _unmovable_display_datas_14)
	TILE_SPRITE_LINE(SPR_LARGEHQ_SOUTH,        _unmovable_display_nothing)

	TILE_SPRITE_LINE(SPR_HUGEHQ_NORTH_GROUND,  _unmovable_display_datas_16)
	TILE_SPRITE_LINE(SPR_HUGEHQ_EAST_GROUND,   _unmovable_display_datas_17)
	TILE_SPRITE_LINE(SPR_HUGEHQ_WEST_GROUND,   _unmovable_display_datas_18)
	TILE_SPRITE_LINE(SPR_HUGEHQ_SOUTH,         _unmovable_display_nothing)
};

#undef TILE_SPRITE_LINE

static const UnmovableSpec _original_unmovable[] = {
	{STR_LAI_UNMOVABLE_DESCRIPTION_TRANSMITTER,          1,   1},
	{STR_LAI_UNMOVABLE_DESCRIPTION_LIGHTHOUSE,           1,   1},
	{STR_TOWN_BUILDING_NAME_STATUE_1,                    1,   1},
	{STR_LAI_UNMOVABLE_DESCRIPTION_COMPANY_OWNED_LAND,   10,  2},
	{STR_LAI_UNMOVABLE_DESCRIPTION_COMPANY_HEADQUARTERS, 1,   1},
};
