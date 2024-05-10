with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Raylib; use Raylib;
with RGUI.Component; use RGUI.Component;

package Cal is

    package AC renames Ada.Calendar;
    type Week_Number is range 0..6;

    function month_start(ntime : Time) return Integer;
    function days_in_month(year: Year_Number; month: Month_Number) return Integer;

    type Calendar_T is new Component_T with record
        fnt        : Font    := GetFontDefault;          
        now        : Time    := AC.Clock;
        start_day  : Integer := month_start(AC.Clock);
        month_days : Integer := days_in_month(AC.Year(AC.Clock), AC.Month(AC.Clock));
    end record;

    overriding procedure Draw(self: in out Calendar_T);
    overriding procedure Update(self: in out Calendar_T);
    procedure CalendarIncrement(self: in out Calendar_T);

    function CalendarCellHovered(self: Calendar_T) return Integer;

private

    fnt_size : Float := 20.0;
    cell_width  : Integer := 33;
    cell_height : Integer := 33;

end Cal;
