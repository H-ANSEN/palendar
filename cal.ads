with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Raylib; use Raylib;

package Cal is

    type Calendar_T is tagged private;

    procedure CalendarDestory(cal: Calendar_T);
    procedure Draw(self: in out Calendar_T);
    function CalendarCellClicked(self: Calendar_T) return Integer;
    function CalendarMake(pos: Vector2) return Calendar_T; 

private
    package AC renames Ada.Calendar;
    type Week_Number is range 0..6;

    function month_start(ntime : Time) return Integer;
    function days_in_month(year: Year_Number; month: Month_Number) return Integer;

    type Calendar_T is tagged record
        pos : Vector2;       -- Top left origin position
        tex : RenderTexture; -- Texture Calendar is drawn to
        fnt : Font;          -- Font used in drawing

        dirty      : Boolean := True;
        now        : Time    := AC.Clock;
        start_day  : Integer := month_start(AC.Clock);
        month_days : Integer := days_in_month(AC.Year(AC.Clock), AC.Month(AC.Clock));
    end record;

    fnt_size : Float := 20.0;
    cell_width  : Integer := 33;
    cell_height : Integer := 33;

end Cal;
