/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file graph_gui.cpp GUI that shows performance graphs. */

#include "stdafx.h"
#include "openttd.h"
#include "gui.h"
#include "window_gui.h"
#include "company_base.h"
#include "company_gui.h"
#include "economy_func.h"
#include "cargotype.h"
#include "strings_func.h"
#include "window_func.h"
#include "date_func.h"
#include "gfx_func.h"
#include "sortlist_type.h"

#include "table/strings.h"
#include "table/sprites.h"

/* Bitmasks of company and cargo indices that shouldn't be drawn. */
static uint _legend_excluded_companies;
static uint _legend_excluded_cargo;

/* Apparently these don't play well with enums. */
static const OverflowSafeInt64 INVALID_DATAPOINT(INT64_MAX); // Value used for a datapoint that shouldn't be drawn.
static const uint INVALID_DATAPOINT_POS = UINT_MAX;  // Used to determine if the previous point was drawn.

/****************/
/* GRAPH LEGEND */
/****************/

/** Widget numbers of the graph legend window. */
enum GraphLegendWidgetNumbers {
	GLW_CLOSEBOX,
	GLW_CAPTION,
	GLW_BACKGROUND,

	GLW_FIRST_COMPANY,
	GLW_LAST_COMPANY = GLW_FIRST_COMPANY + MAX_COMPANIES - 1,
};

struct GraphLegendWindow : Window {
	GraphLegendWindow(const WindowDesc *desc, WindowNumber window_number) : Window()
	{
		this->InitNested(desc, window_number);

		for (CompanyID c = COMPANY_FIRST; c < MAX_COMPANIES; c++) {
			if (!HasBit(_legend_excluded_companies, c)) this->LowerWidget(c + GLW_FIRST_COMPANY);

			this->OnInvalidateData(c);
		}
	}

	virtual void OnPaint()
	{
		this->DrawWidgets();
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		if (!IsInsideMM(widget, GLW_FIRST_COMPANY, MAX_COMPANIES + GLW_FIRST_COMPANY)) return;

		CompanyID cid = (CompanyID)(widget - GLW_FIRST_COMPANY);

		if (!Company::IsValidID(cid)) return;

		DrawCompanyIcon(cid, r.left + 2, r.top + 2);

		SetDParam(0, cid);
		SetDParam(1, cid);
		DrawString(r.left + 19, r.right - 2, r.top + 1, STR_COMPANY_NAME_COMPANY_NUM, HasBit(_legend_excluded_companies, cid) ? TC_BLACK : TC_WHITE);
	}

	virtual void OnClick(Point pt, int widget)
	{
		if (!IsInsideMM(widget, GLW_FIRST_COMPANY, MAX_COMPANIES + GLW_FIRST_COMPANY)) return;

		ToggleBit(_legend_excluded_companies, widget - GLW_FIRST_COMPANY);
		this->ToggleWidgetLoweredState(widget);
		this->SetDirty();
		InvalidateWindow(WC_INCOME_GRAPH, 0);
		InvalidateWindow(WC_OPERATING_PROFIT, 0);
		InvalidateWindow(WC_DELIVERED_CARGO, 0);
		InvalidateWindow(WC_PERFORMANCE_HISTORY, 0);
		InvalidateWindow(WC_COMPANY_VALUE, 0);
	}

	virtual void OnInvalidateData(int data)
	{
		if (Company::IsValidID(data)) return;

		SetBit(_legend_excluded_companies, data);
		this->RaiseWidget(data + GLW_FIRST_COMPANY);
	}
};

/**
 * Construct a vertical list of buttons, one for each company.
 * @param biggest_index Storage for collecting the biggest index used in the returned tree.
 * @return Panel with company buttons.
 * @postcond \c *biggest_index contains the largest used index in the tree.
 */
static NWidgetBase *MakeNWidgetCompanyLines(int *biggest_index)
{
	NWidgetVertical *vert = new NWidgetVertical();

	for (int widnum = GLW_FIRST_COMPANY; widnum <= GLW_LAST_COMPANY; widnum++) {
		NWidgetBackground *panel = new NWidgetBackground(WWT_PANEL, COLOUR_GREY, widnum);
		panel->SetMinimalSize(246, 12);
		panel->SetFill(false, false);
		panel->SetDataTip(0x0, STR_GRAPH_KEY_COMPANY_SELECTION_TOOLTIP);
		vert->Add(panel);
	}
	*biggest_index = GLW_LAST_COMPANY;
	return vert;
}

static const NWidgetPart _nested_graph_legend_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, GLW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, GLW_CAPTION), SetDataTip(STR_GRAPH_KEY_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, GLW_BACKGROUND),
		NWidget(NWID_SPACER), SetMinimalSize(0, 2),
		NWidget(NWID_HORIZONTAL),
			NWidget(NWID_SPACER), SetMinimalSize(2, 0),
			NWidgetFunction(MakeNWidgetCompanyLines),
			NWidget(NWID_SPACER), SetMinimalSize(2, 0),
		EndContainer(),
	EndContainer(),
};

static const WindowDesc _graph_legend_desc(
	WDP_AUTO, WDP_AUTO, 250, 196, 250, 196,
	WC_GRAPH_LEGEND, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET,
	NULL, _nested_graph_legend_widgets, lengthof(_nested_graph_legend_widgets)
);

static void ShowGraphLegend()
{
	AllocateWindowDescFront<GraphLegendWindow>(&_graph_legend_desc, 0);
}

/******************/
/* BASE OF GRAPHS */
/*****************/

/** Widget numbers of a base graph window. */
enum CompanyValueWidgets {
	BGW_CLOSEBOX,
	BGW_CAPTION,
	BGW_KEY_BUTTON,
	BGW_BACKGROUND,
};

struct BaseGraphWindow : Window {
protected:
	enum {
		GRAPH_MAX_DATASETS = 32,
		GRAPH_AXIS_LINE_COLOUR  = 215,
		GRAPH_NUM_MONTHS = 24, ///< Number of months displayed in the graph.

		GRAPH_X_POSITION_BEGINNING  = 44,  ///< Start the graph 44 pixels from gd_left
		GRAPH_X_POSITION_SEPARATION = 22,  ///< There are 22 pixels between each X value

		GRAPH_NUM_LINES_Y = 9, ///< How many horizontal lines to draw.
		/* 9 is convenient as that means the distance between them is the gd_height of the graph / 8,
		 * which is the same
		 * as height >> 3. */
	};

	uint excluded_data; ///< bitmask of the datasets that shouldn't be displayed.
	byte num_dataset;
	byte num_on_x_axis;
	bool has_negative_values;
	byte num_vert_lines;
	static const TextColour graph_axis_label_colour = TC_BLACK; ///< colour of the graph axis label.

	/* The starting month and year that values are plotted against. If month is
	 * 0xFF, use x_values_start and x_values_increment below instead. */
	byte month;
	Year year;

	/* These values are used if the graph is being plotted against values
	 * rather than the dates specified by month and year. */
	uint16 x_values_start;
	uint16 x_values_increment;

	Rect graph_location;
	StringID format_str_y_axis;
	byte colours[GRAPH_MAX_DATASETS];
	OverflowSafeInt64 cost[GRAPH_MAX_DATASETS][GRAPH_NUM_MONTHS]; ///< Stored costs for the last #GRAPH_NUM_MONTHS months

	/**
	 * Actually draw the graph.
	 * @param r the rectangle of the data field of the graph
	 */
	void DrawGraph(Rect &r) const
	{
		uint x, y;                       ///< Reused whenever x and y coordinates are needed.
		OverflowSafeInt64 highest_value; ///< Highest value to be drawn.
		int x_axis_offset;               ///< Distance from the top of the graph to the x axis.

		/* the colours and cost array of GraphDrawer must accomodate
		 * both values for cargo and companies. So if any are higher, quit */
		assert_compile(GRAPH_MAX_DATASETS >= (int)NUM_CARGO && GRAPH_MAX_DATASETS >= (int)MAX_COMPANIES);
		assert(this->num_vert_lines > 0);

		byte grid_colour = _colour_gradient[COLOUR_GREY][4];

		/* The coordinates of the opposite edges of the graph. */
		assert(r.left + GRAPH_X_POSITION_BEGINNING + this->num_vert_lines * GRAPH_X_POSITION_SEPARATION - 1 == r.right);
		int height = r.bottom - r.top + 1;

		/* Draw the vertical grid lines. */

		/* Don't draw the first line, as that's where the axis will be. */
		x = r.left + GRAPH_X_POSITION_BEGINNING + GRAPH_X_POSITION_SEPARATION;

		for (int i = 0; i < this->num_vert_lines; i++) {
			GfxFillRect(x, r.top, x, r.bottom, grid_colour);
			x += GRAPH_X_POSITION_SEPARATION;
		}

		/* Draw the horizontal grid lines. */
		x = r.left + GRAPH_X_POSITION_BEGINNING;
		y = r.bottom;

		for (int i = 0; i < GRAPH_NUM_LINES_Y; i++) {
			GfxFillRect(x, y, r.right, y, grid_colour);
			y -= height / (GRAPH_NUM_LINES_Y - 1);
		}

		/* Draw the y axis. */
		GfxFillRect(x, r.top, x, r.bottom, GRAPH_AXIS_LINE_COLOUR);

		/* Find the distance from the gd_top of the graph to the x axis. */
		x_axis_offset = height;

		/* The graph is currently symmetrical about the x axis. */
		if (this->has_negative_values) x_axis_offset /= 2;

		/* Draw the x axis. */
		y = x_axis_offset + r.top;
		GfxFillRect(x, y, r.right, y, GRAPH_AXIS_LINE_COLOUR);

		/* Find the largest value that will be drawn. */
		if (this->num_on_x_axis == 0)
			return;

		assert(this->num_on_x_axis > 0);
		assert(this->num_dataset > 0);

		/* Start of with a value of twice the gd_height of the graph in pixels. It's a
		 * bit arbitrary, but it makes the cargo payment graph look a little nicer,
		 * and prevents division by zero when calculating where the datapoint
		 * should be drawn. */
		highest_value = x_axis_offset * 2;

		for (int i = 0; i < this->num_dataset; i++) {
			if (!HasBit(this->excluded_data, i)) {
				for (int j = 0; j < this->num_on_x_axis; j++) {
					OverflowSafeInt64 datapoint = this->cost[i][j];

					if (datapoint != INVALID_DATAPOINT) {
						/* For now, if the graph has negative values the scaling is
						 * symmetrical about the x axis, so take the absolute value
						 * of each data point. */
						highest_value = max(highest_value, abs(datapoint));
					}
				}
			}
		}

		/* Round up highest_value so that it will divide cleanly into the number of
		 * axis labels used. */
		int round_val = highest_value % (GRAPH_NUM_LINES_Y - 1);
		if (round_val != 0) highest_value += (GRAPH_NUM_LINES_Y - 1 - round_val);

		/* draw text strings on the y axis */
		int64 y_label = highest_value;
		int64 y_label_separation = highest_value / (GRAPH_NUM_LINES_Y - 1);

		/* If there are negative values, the graph goes from highest_value to
		 * -highest_value, not highest_value to 0. */
		if (this->has_negative_values) y_label_separation *= 2;

		x = r.left + GRAPH_X_POSITION_BEGINNING + 1;
		y = r.top - 3;

		for (int i = 0; i < GRAPH_NUM_LINES_Y; i++) {
			SetDParam(0, this->format_str_y_axis);
			SetDParam(1, y_label);
			DrawString(x - GRAPH_X_POSITION_BEGINNING, x, y, STR_GRAPH_Y_LABEL, graph_axis_label_colour, SA_RIGHT);

			y_label -= y_label_separation;
			y += height / (GRAPH_NUM_LINES_Y - 1);
		}

		/* draw strings on the x axis */
		if (this->month != 0xFF) {
			x = r.left + GRAPH_X_POSITION_BEGINNING;
			y = r.bottom + 2;
			byte month = this->month;
			Year year  = this->year;
			for (int i = 0; i < this->num_on_x_axis; i++) {
				SetDParam(0, month + STR_MONTH_ABBREV_JAN);
				SetDParam(1, month + STR_MONTH_ABBREV_JAN + 2);
				SetDParam(2, year);
				DrawStringMultiLine(x, x + GRAPH_X_POSITION_SEPARATION, y, this->height, month == 0 ? STR_GRAPH_X_LABEL_MONTH_YEAR : STR_GRAPH_X_LABEL_MONTH, graph_axis_label_colour);

				month += 3;
				if (month >= 12) {
					month = 0;
					year++;
				}
				x += GRAPH_X_POSITION_SEPARATION;
			}
		} else {
			/* Draw the label under the data point rather than on the grid line. */
			x = r.left + GRAPH_X_POSITION_BEGINNING;
			y = r.bottom + 2;
			uint16 label = this->x_values_start;

			for (int i = 0; i < this->num_on_x_axis; i++) {
				SetDParam(0, label);
				DrawString(x + 1, x + GRAPH_X_POSITION_SEPARATION - 1, y, STR_GRAPH_Y_LABEL_NUMBER, graph_axis_label_colour, SA_CENTER);

				label += this->x_values_increment;
				x += GRAPH_X_POSITION_SEPARATION;
			}
		}

		/* draw lines and dots */
		for (int i = 0; i < this->num_dataset; i++) {
			if (!HasBit(this->excluded_data, i)) {
				/* Centre the dot between the grid lines. */
				x = r.left + GRAPH_X_POSITION_BEGINNING + (GRAPH_X_POSITION_SEPARATION / 2);

				byte colour  = this->colours[i];
				uint prev_x = INVALID_DATAPOINT_POS;
				uint prev_y = INVALID_DATAPOINT_POS;

				for (int j = 0; j < this->num_on_x_axis; j++) {
					OverflowSafeInt64 datapoint = this->cost[i][j];

					if (datapoint != INVALID_DATAPOINT) {
						/*
						 * Check whether we need to reduce the 'accuracy' of the
						 * datapoint value and the highest value to splut overflows.
						 * And when 'drawing' 'one million' or 'one million and one'
						 * there is no significant difference, so the least
						 * significant bits can just be removed.
						 *
						 * If there are more bits needed than would fit in a 32 bits
						 * integer, so at about 31 bits because of the sign bit, the
						 * least significant bits are removed.
						 */
						int mult_range = FindLastBit(x_axis_offset) + FindLastBit(abs(datapoint));
						int reduce_range = max(mult_range - 31, 0);

						/* Handle negative values differently (don't shift sign) */
						if (datapoint < 0) {
							datapoint = -(abs(datapoint) >> reduce_range);
						} else {
							datapoint >>= reduce_range;
						}

						y = r.top + x_axis_offset - (x_axis_offset * datapoint) / (highest_value >> reduce_range);

						/* Draw the point. */
						GfxFillRect(x - 1, y - 1, x + 1, y + 1, colour);

						/* Draw the line connected to the previous point. */
						if (prev_x != INVALID_DATAPOINT_POS) GfxDrawLine(prev_x, prev_y, x, y, colour);

						prev_x = x;
						prev_y = y;
					} else {
						prev_x = INVALID_DATAPOINT_POS;
						prev_y = INVALID_DATAPOINT_POS;
					}

					x += GRAPH_X_POSITION_SEPARATION;
				}
			}
		}
	}


	BaseGraphWindow(const WindowDesc *desc, WindowNumber window_number, int left,
									int top, int height, bool has_negative_values, StringID format_str_y_axis) :
			Window(desc, window_number), has_negative_values(has_negative_values),
			format_str_y_axis(format_str_y_axis)
	{
		InvalidateWindow(WC_GRAPH_LEGEND, 0);
		this->num_vert_lines = 24;

		this->graph_location.left   = left;
		this->graph_location.right  = left + GRAPH_X_POSITION_BEGINNING + this->num_vert_lines * GRAPH_X_POSITION_SEPARATION - 1;

		this->graph_location.top    = top;
		this->graph_location.bottom = top + height - 1;
	}

	void InitializeWindow(const WindowDesc *desc)
	{
		this->FindWindowPlacementAndResize(desc);

		/* Initialise the dataset */
		this->UpdateStatistics(true);
	}

public:
	virtual void OnPaint()
	{
		this->DrawWidgets();

		this->DrawGraph(this->graph_location);
	}

	virtual OverflowSafeInt64 GetGraphData(const Company *c, int j)
	{
		return INVALID_DATAPOINT;
	}

	virtual void OnClick(Point pt, int widget)
	{
		/* Clicked on legend? */
		if (widget == BGW_KEY_BUTTON) ShowGraphLegend();
	}

	virtual void OnTick()
	{
		this->UpdateStatistics(false);
	}

	/**
	 * Update the statistics.
	 * @param initialize Initialize the data structure.
	 */
	void UpdateStatistics(bool initialize)
	{
		uint excluded_companies = _legend_excluded_companies;

		/* Exclude the companies which aren't valid */
		for (CompanyID c = COMPANY_FIRST; c < MAX_COMPANIES; c++) {
			if (!Company::IsValidID(c)) SetBit(excluded_companies, c);
		}

		byte nums = 0;
		const Company *c;
		FOR_ALL_COMPANIES(c) {
			nums = min(this->num_vert_lines, max(nums, c->num_valid_stat_ent));
		}

		int mo = (_cur_month / 3 - nums) * 3;
		int yr = _cur_year;
		while (mo < 0) {
			yr--;
			mo += 12;
		}

		if (!initialize && this->excluded_data == excluded_companies && this->num_on_x_axis == nums &&
				this->year == yr && this->month == mo) {
			/* There's no reason to get new stats */
			return;
		}

		this->excluded_data = excluded_companies;
		this->num_on_x_axis = nums;
		this->year = yr;
		this->month = mo;

		int numd = 0;
		for (CompanyID k = COMPANY_FIRST; k < MAX_COMPANIES; k++) {
			c = Company::GetIfValid(k);
			if (c != NULL) {
				this->colours[numd] = _colour_gradient[c->colour][6];
				for (int j = this->num_on_x_axis, i = 0; --j >= 0;) {
					this->cost[numd][i] = (j >= c->num_valid_stat_ent) ? INVALID_DATAPOINT : GetGraphData(c, j);
					i++;
				}
			}
			numd++;
		}

		this->num_dataset = numd;
	}
};


/********************/
/* OPERATING PROFIT */
/********************/

struct OperatingProfitGraphWindow : BaseGraphWindow {
	OperatingProfitGraphWindow(const WindowDesc *desc, WindowNumber window_number) :
			BaseGraphWindow(desc, window_number, 2, 18, 136, true, STR_JUST_CURRCOMPACT)
	{
		this->InitializeWindow(desc);
	}

	virtual OverflowSafeInt64 GetGraphData(const Company *c, int j)
	{
		return c->old_economy[j].income + c->old_economy[j].expenses;
	}
};

static const Widget _operating_profit_widgets[] = {
{   WWT_CLOSEBOX,   RESIZE_NONE,  COLOUR_GREY,     0,    10,     0,    13, STR_BLACK_CROSS,                    STR_TOOLTIP_CLOSE_WINDOW},
{    WWT_CAPTION,   RESIZE_NONE,  COLOUR_GREY,    11,   525,     0,    13, STR_GRAPH_OPERATING_PROFIT_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},
{ WWT_PUSHTXTBTN,   RESIZE_NONE,  COLOUR_GREY,   526,   575,     0,    13, STR_GRAPH_KEY_BUTTON,               STR_GRAPH_KEY_TOOLTIP},
{      WWT_PANEL,   RESIZE_NONE,  COLOUR_GREY,     0,   575,    14,   173, 0x0,                                STR_NULL},
{   WIDGETS_END},
};

static const NWidgetPart _nested_operating_profit_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, BGW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, BGW_CAPTION), SetDataTip(STR_GRAPH_OPERATING_PROFIT_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, BGW_KEY_BUTTON), SetMinimalSize(50, 14), SetDataTip(STR_GRAPH_KEY_BUTTON, STR_GRAPH_KEY_TOOLTIP),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, BGW_BACKGROUND), SetMinimalSize(576, 160), EndContainer(),
};

static const WindowDesc _operating_profit_desc(
	WDP_AUTO, WDP_AUTO, 576, 174, 576, 174,
	WC_OPERATING_PROFIT, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET | WDF_UNCLICK_BUTTONS,
	_operating_profit_widgets, _nested_operating_profit_widgets, lengthof(_nested_operating_profit_widgets)
);


void ShowOperatingProfitGraph()
{
	AllocateWindowDescFront<OperatingProfitGraphWindow>(&_operating_profit_desc, 0);
}


/****************/
/* INCOME GRAPH */
/****************/

struct IncomeGraphWindow : BaseGraphWindow {
	IncomeGraphWindow(const WindowDesc *desc, WindowNumber window_number) :
			BaseGraphWindow(desc, window_number, 2, 18, 104, false, STR_JUST_CURRCOMPACT)
	{
		this->InitializeWindow(desc);
	}

	virtual OverflowSafeInt64 GetGraphData(const Company *c, int j)
	{
		return c->old_economy[j].income;
	}
};

static const Widget _income_graph_widgets[] = {
{   WWT_CLOSEBOX,   RESIZE_NONE,  COLOUR_GREY,     0,    10,     0,    13, STR_BLACK_CROSS,          STR_TOOLTIP_CLOSE_WINDOW},
{    WWT_CAPTION,   RESIZE_NONE,  COLOUR_GREY,    11,   525,     0,    13, STR_GRAPH_INCOME_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},
{ WWT_PUSHTXTBTN,   RESIZE_NONE,  COLOUR_GREY,   526,   575,     0,    13, STR_GRAPH_KEY_BUTTON,     STR_GRAPH_KEY_TOOLTIP},
{      WWT_PANEL,   RESIZE_NONE,  COLOUR_GREY,     0,   575,    14,   141, 0x0,                      STR_NULL},
{   WIDGETS_END},
};

static const NWidgetPart _nested_income_graph_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, BGW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, BGW_CAPTION), SetDataTip(STR_GRAPH_INCOME_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, BGW_KEY_BUTTON), SetMinimalSize(50, 14), SetDataTip(STR_GRAPH_KEY_BUTTON, STR_GRAPH_KEY_TOOLTIP),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, BGW_BACKGROUND), SetMinimalSize(576, 128), EndContainer(),
};


static const WindowDesc _income_graph_desc(
	WDP_AUTO, WDP_AUTO, 576, 142, 576, 142,
	WC_INCOME_GRAPH, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET | WDF_UNCLICK_BUTTONS,
	_income_graph_widgets, _nested_income_graph_widgets, lengthof(_nested_income_graph_widgets)
);

void ShowIncomeGraph()
{
	AllocateWindowDescFront<IncomeGraphWindow>(&_income_graph_desc, 0);
}

/*******************/
/* DELIVERED CARGO */
/*******************/

struct DeliveredCargoGraphWindow : BaseGraphWindow {
	DeliveredCargoGraphWindow(const WindowDesc *desc, WindowNumber window_number) :
			BaseGraphWindow(desc, window_number, 2, 18, 104, false, STR_JUST_COMMA)
	{
		this->InitializeWindow(desc);
	}

	virtual OverflowSafeInt64 GetGraphData(const Company *c, int j)
	{
		return c->old_economy[j].delivered_cargo;
	}
};

static const Widget _delivered_cargo_graph_widgets[] = {
{   WWT_CLOSEBOX,   RESIZE_NONE,  COLOUR_GREY,     0,    10,     0,    13, STR_BLACK_CROSS,                   STR_TOOLTIP_CLOSE_WINDOW},
{    WWT_CAPTION,   RESIZE_NONE,  COLOUR_GREY,    11,   525,     0,    13, STR_GRAPH_CARGO_DELIVERED_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},
{ WWT_PUSHTXTBTN,   RESIZE_NONE,  COLOUR_GREY,   526,   575,     0,    13, STR_GRAPH_KEY_BUTTON,              STR_GRAPH_KEY_TOOLTIP},
{      WWT_PANEL,   RESIZE_NONE,  COLOUR_GREY,     0,   575,    14,   141, 0x0,                               STR_NULL},
{   WIDGETS_END},
};

static const NWidgetPart _nested_delivered_cargo_graph_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, BGW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, BGW_CAPTION), SetDataTip(STR_GRAPH_CARGO_DELIVERED_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, BGW_KEY_BUTTON), SetMinimalSize(50, 14), SetDataTip(STR_GRAPH_KEY_BUTTON, STR_GRAPH_KEY_TOOLTIP),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, BGW_BACKGROUND), SetMinimalSize(576, 128), EndContainer(),
};

static const WindowDesc _delivered_cargo_graph_desc(
	WDP_AUTO, WDP_AUTO, 576, 142, 576, 142,
	WC_DELIVERED_CARGO, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET | WDF_UNCLICK_BUTTONS,
	_delivered_cargo_graph_widgets, _nested_delivered_cargo_graph_widgets, lengthof(_nested_delivered_cargo_graph_widgets)
);

void ShowDeliveredCargoGraph()
{
	AllocateWindowDescFront<DeliveredCargoGraphWindow>(&_delivered_cargo_graph_desc, 0);
}

/***********************/
/* PERFORMANCE HISTORY */
/***********************/

/** Widget numbers of the performance history window. */
enum PerformanceHistoryGraphWidgets {
	PHW_CLOSEBOX,
	PHW_CAPTION,
	PHW_KEY,
	PHW_DETAILED_PERFORMANCE,
	PHW_BACKGROUND,
};

struct PerformanceHistoryGraphWindow : BaseGraphWindow {
	PerformanceHistoryGraphWindow(const WindowDesc *desc, WindowNumber window_number) :
			BaseGraphWindow(desc, window_number, 2, 18, 200, false, STR_JUST_COMMA)
	{
		this->InitializeWindow(desc);
	}

	virtual OverflowSafeInt64 GetGraphData(const Company *c, int j)
	{
		return c->old_economy[j].performance_history;
	}

	virtual void OnClick(Point pt, int widget)
	{
		if (widget == PHW_DETAILED_PERFORMANCE) ShowPerformanceRatingDetail();
		this->BaseGraphWindow::OnClick(pt, widget);
	}
};

static const Widget _performance_history_widgets[] = {
{   WWT_CLOSEBOX,   RESIZE_NONE,  COLOUR_GREY,     0,    10,     0,    13, STR_BLACK_CROSS,                               STR_TOOLTIP_CLOSE_WINDOW},              // PHW_CLOSEBOX
{    WWT_CAPTION,   RESIZE_NONE,  COLOUR_GREY,    11,   475,     0,    13, STR_GRAPH_COMPANY_PERFORMANCE_RATINGS_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},    // PHW_CAPTION
{ WWT_PUSHTXTBTN,   RESIZE_NONE,  COLOUR_GREY,   526,   575,     0,    13, STR_GRAPH_KEY_BUTTON,                          STR_GRAPH_KEY_TOOLTIP},                 // PHW_KEY
{ WWT_PUSHTXTBTN,   RESIZE_NONE,  COLOUR_GREY,   476,   525,     0,    13, STR_PERFORMANCE_DETAIL_KEY,                    STR_GRAPH_PERFORMANCE_DETAIL_TOOLTIP}, // PHW_DETAILED_PERFORMANCE
{      WWT_PANEL,   RESIZE_NONE,  COLOUR_GREY,     0,   575,    14,   237, 0x0,                                           STR_NULL},                              // PHW_BACKGROUND
{   WIDGETS_END},
};

static const NWidgetPart _nested_performance_history_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, PHW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, PHW_CAPTION), SetDataTip(STR_GRAPH_COMPANY_PERFORMANCE_RATINGS_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, PHW_DETAILED_PERFORMANCE), SetMinimalSize(50, 14), SetDataTip(STR_PERFORMANCE_DETAIL_KEY, STR_GRAPH_PERFORMANCE_DETAIL_TOOLTIP),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, PHW_KEY), SetMinimalSize(50, 14), SetDataTip(STR_GRAPH_KEY_BUTTON, STR_GRAPH_KEY_TOOLTIP),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, PHW_BACKGROUND), SetMinimalSize(576, 224), EndContainer(),
};

static const WindowDesc _performance_history_desc(
	WDP_AUTO, WDP_AUTO, 576, 238, 576, 238,
	WC_PERFORMANCE_HISTORY, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET | WDF_UNCLICK_BUTTONS,
	_performance_history_widgets, _nested_performance_history_widgets, lengthof(_nested_performance_history_widgets)
);

void ShowPerformanceHistoryGraph()
{
	AllocateWindowDescFront<PerformanceHistoryGraphWindow>(&_performance_history_desc, 0);
}

/*****************/
/* COMPANY VALUE */
/*****************/

struct CompanyValueGraphWindow : BaseGraphWindow {
	CompanyValueGraphWindow(const WindowDesc *desc, WindowNumber window_number) :
			BaseGraphWindow(desc, window_number, 2, 18, 200, false, STR_JUST_CURRCOMPACT)
	{
		this->InitializeWindow(desc);
	}

	virtual OverflowSafeInt64 GetGraphData(const Company *c, int j)
	{
		return c->old_economy[j].company_value;
	}
};

static const Widget _company_value_graph_widgets[] = {
{   WWT_CLOSEBOX,   RESIZE_NONE,  COLOUR_GREY,     0,    10,     0,    13, STR_BLACK_CROSS,                  STR_TOOLTIP_CLOSE_WINDOW},
{    WWT_CAPTION,   RESIZE_NONE,  COLOUR_GREY,    11,   525,     0,    13, STR_GRAPH_COMPANY_VALUES_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},
{ WWT_PUSHTXTBTN,   RESIZE_NONE,  COLOUR_GREY,   526,   575,     0,    13, STR_GRAPH_KEY_BUTTON,             STR_GRAPH_KEY_TOOLTIP},
{      WWT_PANEL,   RESIZE_NONE,  COLOUR_GREY,     0,   575,    14,   237, 0x0,                              STR_NULL},
{   WIDGETS_END},
};

static const NWidgetPart _nested_company_value_graph_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, BGW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, BGW_CAPTION), SetDataTip(STR_GRAPH_COMPANY_VALUES_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_PUSHTXTBTN, COLOUR_GREY, BGW_KEY_BUTTON), SetMinimalSize(50, 14), SetDataTip(STR_GRAPH_KEY_BUTTON, STR_GRAPH_KEY_TOOLTIP),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, BGW_BACKGROUND), SetMinimalSize(576, 224), EndContainer(),
};

static const WindowDesc _company_value_graph_desc(
	WDP_AUTO, WDP_AUTO, 576, 238, 576, 238,
	WC_COMPANY_VALUE, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET | WDF_UNCLICK_BUTTONS,
	_company_value_graph_widgets, _nested_company_value_graph_widgets, lengthof(_nested_company_value_graph_widgets)
);

void ShowCompanyValueGraph()
{
	AllocateWindowDescFront<CompanyValueGraphWindow>(&_company_value_graph_desc, 0);
}

/*****************/
/* PAYMENT RATES */
/*****************/

/** Widget numbers of the cargo payment rates. */
enum CargoPaymentRatesWidgets {
	CPW_CLOSEBOX,
	CPW_CAPTION,
	CPW_BACKGROUND,
	CPW_CARGO_FIRST,
};

struct PaymentRatesGraphWindow : BaseGraphWindow {
	PaymentRatesGraphWindow(const WindowDesc *desc, WindowNumber window_number) :
			BaseGraphWindow(desc, window_number, 2, 24, 200, false, STR_JUST_CURRCOMPACT)
	{
		uint num_active = 0;
		const CargoSpec *cs;
		FOR_ALL_CARGOSPECS(cs) {
			num_active++;
		}

		/* Resize the window to fit the cargo types */
		ResizeWindow(this, 0, max(num_active, 12U) * 8);

		/* Add widgets for each cargo type */
		this->widget_count += num_active;
		this->widget = ReallocT(this->widget, this->widget_count + 1);
		this->widget[this->widget_count].type = WWT_LAST;

		/* Set the properties of each widget */
		for (uint i = 0; i != num_active; i++) {
			Widget *wi = &this->widget[CPW_CARGO_FIRST + i];
			wi->type     = WWT_PANEL;
			wi->display_flags = RESIZE_NONE;
			wi->colour   = COLOUR_ORANGE;
			wi->left     = 493;
			wi->right    = 562;
			wi->top      = 24 + i * 8;
			wi->bottom   = wi->top + 7;
			wi->data     = 0;
			wi->tooltips = STR_GRAPH_CARGO_PAYMENT_TOGGLE_CARGO;

			if (!HasBit(_legend_excluded_cargo, i)) this->LowerWidget(i + CPW_CARGO_FIRST);
		}

		this->SetDirty();

		this->num_on_x_axis = 20;
		this->num_vert_lines = 20;
		this->month = 0xFF;
		this->x_values_start     = 10;
		this->x_values_increment = 10;

		this->graph_location.right  = this->graph_location.left + GRAPH_X_POSITION_BEGINNING + this->num_vert_lines * GRAPH_X_POSITION_SEPARATION - 1;
		this->graph_location.bottom = this->graph_location.top + (this->height - 38) - 1;

		/* Initialise the dataset */
		this->OnHundredthTick();

		this->FindWindowPlacementAndResize(desc);
	}

	virtual void OnPaint()
	{
		this->DrawWidgets();

		int x = 495;
		int y = 24;

		uint i = 0;

		const CargoSpec *cs;
		FOR_ALL_CARGOSPECS(cs) {
			/* Only draw labels for widgets that exist. If the widget doesn't
			 * exist then the local company has used the climate cheat or
			 * changed the NewGRF configuration with this window open. */
			if (i + CPW_CARGO_FIRST < this->widget_count) {
				/* Since the buttons have no text, no images,
				 * both the text and the coloured box have to be manually painted.
				 * clk_dif will move one pixel down and one pixel to the right
				 * when the button is clicked */
				byte clk_dif = this->IsWidgetLowered(i + CPW_CARGO_FIRST) ? 1 : 0;

				GfxFillRect(x + clk_dif, y + clk_dif, x + 8 + clk_dif, y + 5 + clk_dif, 0);
				GfxFillRect(x + 1 + clk_dif, y + 1 + clk_dif, x + 7 + clk_dif, y + 4 + clk_dif, cs->legend_colour);
				SetDParam(0, cs->name);
				DrawString(x + 14 + clk_dif, this->width, y + clk_dif, STR_GRAPH_CARGO_PAYMENT_CARGO);
				y += 8;
			}
			i++;
		}

		this->DrawGraph(this->graph_location);

		DrawString(2 + 46, this->width, this->graph_location.bottom + 8, STR_GRAPH_CARGO_PAYMENT_RATES_X_LABEL);
		DrawString(2 + 84, this->width, this->graph_location.top - 9, STR_GRAPH_CARGO_PAYMENT_RATES_TITLE);
	}

	virtual void OnClick(Point pt, int widget)
	{
		if (widget >= CPW_CARGO_FIRST) {
			ToggleBit(_legend_excluded_cargo, widget - CPW_CARGO_FIRST);
			this->ToggleWidgetLoweredState(widget);
			this->excluded_data = _legend_excluded_cargo;
			this->SetDirty();
		}
	}

	virtual void OnTick()
	{
		/* Override default OnTick */
	}

	virtual void OnHundredthTick()
	{
		this->excluded_data = _legend_excluded_cargo;

		int i = 0;
		const CargoSpec *cs;
		FOR_ALL_CARGOSPECS(cs) {
			this->colours[i] = cs->legend_colour;
			for (uint j = 0; j != 20; j++) {
				this->cost[i][j] = GetTransportedGoodsIncome(10, 20, j * 4 + 4, cs->Index());
			}

			i++;
		}
		this->num_dataset = i;
	}
};

static const Widget _cargo_payment_rates_widgets[] = {
{   WWT_CLOSEBOX,   RESIZE_NONE,  COLOUR_GREY,     0,    10,     0,    13, STR_BLACK_CROSS,                       STR_TOOLTIP_CLOSE_WINDOW},
{    WWT_CAPTION,   RESIZE_NONE,  COLOUR_GREY,    11,   567,     0,    13, STR_GRAPH_CARGO_PAYMENT_RATES_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},
{      WWT_PANEL, RESIZE_BOTTOM,  COLOUR_GREY,     0,   567,    14,    45, 0x0,                                   STR_NULL},
{   WIDGETS_END},
};

static const NWidgetPart _nested_cargo_payment_rates_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, CPW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, CPW_CAPTION), SetDataTip(STR_GRAPH_CARGO_PAYMENT_RATES_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, CPW_BACKGROUND), SetMinimalSize(568, 32), SetResize(0, 1), EndContainer(),
};

static const WindowDesc _cargo_payment_rates_desc(
	WDP_AUTO, WDP_AUTO, 568, 46, 568, 46,
	WC_PAYMENT_RATES, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET,
	_cargo_payment_rates_widgets, _nested_cargo_payment_rates_widgets, lengthof(_nested_cargo_payment_rates_widgets)
);


void ShowCargoPaymentRates()
{
	AllocateWindowDescFront<PaymentRatesGraphWindow>(&_cargo_payment_rates_desc, 0);
}

/************************/
/* COMPANY LEAGUE TABLE */
/************************/

static const StringID _performance_titles[] = {
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_ENGINEER,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_ENGINEER,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_TRAFFIC_MANAGER,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_TRAFFIC_MANAGER,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_TRANSPORT_COORDINATOR,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_TRANSPORT_COORDINATOR,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_ROUTE_SUPERVISOR,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_ROUTE_SUPERVISOR,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_DIRECTOR,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_DIRECTOR,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_CHIEF_EXECUTIVE,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_CHIEF_EXECUTIVE,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_CHAIRMAN,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_CHAIRMAN,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_PRESIDENT,
	STR_COMPANY_LEAGUE_PERFORMANCE_TITLE_TYCOON,
};

static inline StringID GetPerformanceTitleFromValue(uint value)
{
	return _performance_titles[minu(value, 1000) >> 6];
}

class CompanyLeagueWindow : public Window {
private:
	GUIList<const Company*> companies;

	/**
	 * (Re)Build the company league list
	 */
	void BuildCompanyList()
	{
		if (!this->companies.NeedRebuild()) return;

		this->companies.Clear();

		const Company *c;
		FOR_ALL_COMPANIES(c) {
			*this->companies.Append() = c;
		}

		this->companies.Compact();
		this->companies.RebuildDone();
	}

	/** Sort the company league by performance history */
	static int CDECL PerformanceSorter(const Company * const *c1, const Company * const *c2)
	{
		return (*c2)->old_economy[1].performance_history - (*c1)->old_economy[1].performance_history;
	}

public:
	CompanyLeagueWindow(const WindowDesc *desc, WindowNumber window_number) : Window(desc, window_number)
	{
		this->companies.ForceRebuild();
		this->companies.NeedResort();

		this->FindWindowPlacementAndResize(desc);
	}

	virtual void OnPaint()
	{
		this->BuildCompanyList();
		this->companies.Sort(&PerformanceSorter);

		this->DrawWidgets();

		for (uint i = 0; i != this->companies.Length(); i++) {
			const Company *c = this->companies[i];
			SetDParam(0, i + STR_ORDINAL_NUMBER_1ST);
			SetDParam(1, c->index);
			SetDParam(2, c->index);
			SetDParam(3, GetPerformanceTitleFromValue(c->old_economy[1].performance_history));

			DrawString(2, this->width, 15 + i * 10, i == 0 ? STR_COMPANY_LEAGUE_FIRST : STR_COMPANY_LEAGUE_OTHER);
			DrawCompanyIcon(c->index, 27, 16 + i * 10);
		}
	}

	virtual void OnTick()
	{
		if (this->companies.NeedResort()) {
			this->SetDirty();
		}
	}

	virtual void OnInvalidateData(int data)
	{
		if (data == 0) {
			this->companies.ForceRebuild();
		} else {
			this->companies.ForceResort();
		}
	}
};

/** Widget numbers for the company league window. */
enum CompanyLeagueWidgets {
	CLW_CLOSEBOX,
	CLW_CAPTION,
	CLW_STICKYBOX,
	CLW_BACKGROUND,
};

static const Widget _company_league_widgets[] = {
{   WWT_CLOSEBOX, RESIZE_NONE,  COLOUR_GREY,   0,  10,  0,  13, STR_BLACK_CROSS,                  STR_TOOLTIP_CLOSE_WINDOW},
{    WWT_CAPTION, RESIZE_NONE,  COLOUR_GREY,  11, 387,  0,  13, STR_COMPANY_LEAGUE_TABLE_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS},
{  WWT_STICKYBOX, RESIZE_NONE,  COLOUR_GREY, 388, 399,  0,  13, STR_NULL,                         STR_TOOLTIP_STICKY},
{      WWT_PANEL, RESIZE_NONE,  COLOUR_GREY,   0, 399, 14, 166, 0x0,                              STR_NULL},
{   WIDGETS_END},
};

static const NWidgetPart _nested_company_league_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, CLW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, CLW_CAPTION), SetDataTip(STR_COMPANY_LEAGUE_TABLE_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_STICKYBOX, COLOUR_GREY, CLW_STICKYBOX),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, CLW_BACKGROUND), SetMinimalSize(400, 153),
};

static const WindowDesc _company_league_desc(
	WDP_AUTO, WDP_AUTO, 400, 167, 400, 167,
	WC_COMPANY_LEAGUE, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET | WDF_STICKY_BUTTON,
	_company_league_widgets, _nested_company_league_widgets, lengthof(_nested_company_league_widgets)
);

void ShowCompanyLeagueTable()
{
	AllocateWindowDescFront<CompanyLeagueWindow>(&_company_league_desc, 0);
}

/*****************************/
/* PERFORMANCE RATING DETAIL */
/*****************************/

/** Widget numbers of the performance rating details window. */
enum PerformanceRatingDetailsWidgets {
	PRW_CLOSEBOX,
	PRW_CAPTION,
	PRW_BACKGROUND,

	PRW_SCORE_FIRST,
	PRW_SCORE_LAST = PRW_SCORE_FIRST + (SCORE_END - SCORE_BEGIN) - 1,

	PRW_COMPANY_FIRST,
	PRW_COMPANY_LAST  = PRW_COMPANY_FIRST + MAX_COMPANIES - 1,
};

struct PerformanceRatingDetailWindow : Window {
	static CompanyID company;
	int timeout;

	PerformanceRatingDetailWindow(const WindowDesc *desc, WindowNumber window_number) : Window()
	{
		this->UpdateCompanyStats();

		this->InitNested(desc, window_number);
		this->OnInvalidateData(INVALID_COMPANY);
	}

	void UpdateCompanyStats()
	{
		/* Update all company stats with the current data
		 * (this is because _score_info is not saved to a savegame) */
		Company *c;
		FOR_ALL_COMPANIES(c) {
			UpdateCompanyRatingAndValue(c, false);
		}

		this->timeout = DAY_TICKS * 5;
	}

	virtual void OnPaint()
	{
		/* Draw standard stuff */
		this->DrawWidgets();
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		/* No need to draw when there's nothing to draw */
		if (this->company == INVALID_COMPANY) return;

		if (IsInsideMM(widget, PRW_COMPANY_FIRST, PRW_COMPANY_LAST + 1)) {
			if (this->IsWidgetDisabled(widget)) return;
			CompanyID cid = (CompanyID)(widget - PRW_COMPANY_FIRST);
			int offset = (cid == this->company) ? 1 : 0;
			Dimension sprite_size = GetSpriteSize(SPR_PLAYER_ICON);
			DrawCompanyIcon(cid, (r.left + r.right - sprite_size.width) / 2 + offset, (r.top + r.bottom - sprite_size.height) / 2 + offset);
			return;
		}

		if (!IsInsideMM(widget, PRW_SCORE_FIRST, PRW_SCORE_LAST + 1)) return;

		ScoreID score_type = (ScoreID)(widget - PRW_SCORE_FIRST);

			/* The colours used to show how the progress is going */
		int colour_done = _colour_gradient[COLOUR_GREEN][4];
		int colour_notdone = _colour_gradient[COLOUR_RED][4];

		/* Draw all the score parts */
		int val    = _score_part[company][score_type];
		int needed = _score_info[score_type].needed;
		int score  = _score_info[score_type].score;

		/* SCORE_TOTAL has his own rules ;) */
		if (score_type == SCORE_TOTAL) {
			for (ScoreID i = SCORE_BEGIN; i < SCORE_END; i++) score += _score_info[i].score;
			needed = SCORE_MAX;
		}

		DrawString(7, 107, r.top + 6, STR_PERFORMANCE_DETAIL_VEHICLES + score_type);

		/* Draw the score */
		SetDParam(0, score);
		DrawString(7, 107, r.top + 6, STR_PERFORMANCE_DETAIL_INT, TC_FROMSTRING, SA_RIGHT);

		/* Calculate the %-bar */
		byte x = Clamp(val, 0, needed) * 50 / needed;

		/* Draw the bar */
		if (x !=  0) GfxFillRect(112,     r.top + 4, 112 + x,  r.top + 16, colour_done);
		if (x != 50) GfxFillRect(112 + x, r.top + 4, 112 + 50, r.top + 16, colour_notdone);

		/* Draw it */
		SetDParam(0, Clamp(val, 0, needed) * 100 / needed);
		DrawString(112, 162, r.top + 6, STR_PERFORMANCE_DETAIL_PERCENT, TC_FROMSTRING, SA_CENTER);

		/* SCORE_LOAN is inversed */
		if (score_type == SCORE_LOAN) val = needed - val;

		/* Draw the amount we have against what is needed
			* For some of them it is in currency format */
		SetDParam(0, val);
		SetDParam(1, needed);
		switch (score_type) {
			case SCORE_MIN_PROFIT:
			case SCORE_MIN_INCOME:
			case SCORE_MAX_INCOME:
			case SCORE_MONEY:
			case SCORE_LOAN:
				DrawString(167, this->width, r.top + 6, STR_PERFORMANCE_DETAIL_AMOUNT_CURRENCY);
				break;
			default:
				DrawString(167, this->width, r.top + 6, STR_PERFORMANCE_DETAIL_AMOUNT_INT);
		}
	}

	virtual void OnClick(Point pt, int widget)
	{
		/* Check which button is clicked */
		if (IsInsideMM(widget, PRW_COMPANY_FIRST, PRW_COMPANY_LAST + 1)) {
			/* Is it no on disable? */
			if (!this->IsWidgetDisabled(widget)) {
				this->RaiseWidget(this->company + PRW_COMPANY_FIRST);
				this->company = (CompanyID)(widget - PRW_COMPANY_FIRST);
				this->LowerWidget(this->company + PRW_COMPANY_FIRST);
				this->SetDirty();
			}
		}
	}

	virtual void OnTick()
	{
		if (_pause_mode != PM_UNPAUSED) return;

		/* Update the company score every 5 days */
		if (--this->timeout == 0) {
			this->UpdateCompanyStats();
			this->SetDirty();
		}
	}

	/**
	 * Invalidate the data of this window.
	 * @param data the company ID of the company that is going to be removed
	 */
	virtual void OnInvalidateData(int data)
	{
		/* Disable the companies who are not active */
		for (CompanyID i = COMPANY_FIRST; i < MAX_COMPANIES; i++) {
			this->SetWidgetDisabledState(i + PRW_COMPANY_FIRST, !Company::IsValidID(i) || i == data);
		}

		/* Check if the currently selected company is still active. */
		if (this->company == data || (this->company != INVALID_COMPANY && !Company::IsValidID(this->company))) {
			/* Raise the widget for the previous selection. */
			this->RaiseWidget(this->company + PRW_COMPANY_FIRST);
			this->company = INVALID_COMPANY;
		}

		if (this->company == INVALID_COMPANY) {
			const Company *c;
			FOR_ALL_COMPANIES(c) {
				if (c->index == data) continue; // Ignore to-be-removed company
				this->company = c->index;
				break;
			}
		}

		/* Make sure the widget is lowered */
		this->LowerWidget(this->company + PRW_COMPANY_FIRST);
	}
};

CompanyID PerformanceRatingDetailWindow::company = INVALID_COMPANY;

/** Make a vertical list of panels for outputting score details.
 * @param biggest_index Storage for collecting the biggest index used in the returned tree.
 * @return Panel with performance details.
 * @postcond \c *biggest_index contains the largest used index in the tree.
 */
static NWidgetBase *MakePerformanceDetailPanels(int *biggest_index)
{
	const StringID performance_tips[] = {
		STR_PERFORMANCE_DETAIL_VEHICLES_TOOLTIP,
		STR_PERFORMANCE_DETAIL_STATIONS_TOOLTIP,
		STR_PERFORMANCE_DETAIL_MIN_PROFIT_TOOLTIP,
		STR_PERFORMANCE_DETAIL_MIN_INCOME_TOOLTIP,
		STR_PERFORMANCE_DETAIL_MAX_INCOME_TOOLTIP,
		STR_PERFORMANCE_DETAIL_DELIVERED_TOOLTIP,
		STR_PERFORMANCE_DETAIL_CARGO_TOOLTIP,
		STR_PERFORMANCE_DETAIL_MONEY_TOOLTIP,
		STR_PERFORMANCE_DETAIL_LOAN_TOOLTIP,
		STR_PERFORMANCE_DETAIL_TOTAL_TOOLTIP,
	};

	assert_compile(lengthof(performance_tips) == SCORE_END - SCORE_BEGIN);

	NWidgetVertical *vert = new NWidgetVertical();
	for (int widnum = PRW_SCORE_FIRST; widnum <= PRW_SCORE_LAST; widnum++) {
		NWidgetBackground *panel = new NWidgetBackground(WWT_PANEL, COLOUR_GREY, widnum);
		panel->SetMinimalSize(299, 20);
		panel->SetFill(false, false);
		panel->SetDataTip(0x0, performance_tips[widnum - PRW_SCORE_FIRST]);
		vert->Add(panel);
	}
	*biggest_index = PRW_SCORE_LAST;
	return vert;
}

/**
 * Make a number of rows with button-like graphics, for enabling/disabling each company.
 * @param biggest_index Storage for collecting the biggest index used in the returned tree.
 * @return Panel with rows of company buttons.
 * @postcond \c *biggest_index contains the largest used index in the tree.
 */
static NWidgetBase *MakeCompanyButtonRows(int *biggest_index)
{
	static const int MAX_LENGTH = 8; // Maximal number of company buttons in one row.
	NWidgetVertical *vert = NULL; // Storage for all rows.
	NWidgetHorizontal *hor = NULL; // Storage for buttons in one row.
	int hor_length = 0;

	Dimension sprite_size = GetSpriteSize(SPR_PLAYER_ICON);
	sprite_size.width  += WD_MATRIX_LEFT + WD_MATRIX_RIGHT;
	sprite_size.height += WD_MATRIX_TOP + WD_MATRIX_BOTTOM + 1; // 1 for the 'offset' of being pressed

	for (int widnum = PRW_COMPANY_FIRST; widnum <= PRW_COMPANY_LAST; widnum++) {
		/* Ensure there is room in 'hor' for another button. */
		if (hor_length == MAX_LENGTH) {
			if (vert == NULL) vert = new NWidgetVertical();
			vert->Add(hor);
			hor = NULL;
			hor_length = 0;
		}
		if (hor == NULL) {
			hor = new NWidgetHorizontal();
			hor_length = 0;
		}

		NWidgetBackground *panel = new NWidgetBackground(WWT_PANEL, COLOUR_GREY, widnum);
		panel->SetMinimalSize(sprite_size.width, sprite_size.height);
		panel->SetFill(true, false);
		panel->SetDataTip(0x0, STR_GRAPH_KEY_COMPANY_SELECTION_TOOLTIP);
		hor->Add(panel);
		hor_length++;
	}
	*biggest_index = PRW_COMPANY_LAST;
	if (vert == NULL) return hor; // All buttons fit in a single row.

	if (hor_length > 0 && hor_length < MAX_LENGTH) {
		/* Last row is partial, add a spacer at the end to force all buttons to the left. */
		NWidgetSpacer *spc = new NWidgetSpacer(0, 0);
		spc->SetMinimalSize(sprite_size.width, sprite_size.height);
		spc->SetFill(true, false);
		hor->Add(spc);
	}
	if (hor != NULL) vert->Add(hor);
	return vert;
}

static const NWidgetPart _nested_performance_rating_detail_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_GREY, PRW_CLOSEBOX),
		NWidget(WWT_CAPTION, COLOUR_GREY, PRW_CAPTION), SetDataTip(STR_PERFORMANCE_DETAIL, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_GREY, PRW_BACKGROUND),
		NWidgetFunction(MakeCompanyButtonRows), SetPadding(0, 1, 1, 2),
	EndContainer(),
	NWidgetFunction(MakePerformanceDetailPanels),
};

static const WindowDesc _performance_rating_detail_desc(
	WDP_AUTO, WDP_AUTO, 299, 241, 299, 241,
	WC_PERFORMANCE_DETAIL, WC_NONE,
	WDF_STD_TOOLTIPS | WDF_STD_BTN | WDF_DEF_WIDGET,
	NULL, _nested_performance_rating_detail_widgets, lengthof(_nested_performance_rating_detail_widgets)
);

void ShowPerformanceRatingDetail()
{
	AllocateWindowDescFront<PerformanceRatingDetailWindow>(&_performance_rating_detail_desc, 0);
}
