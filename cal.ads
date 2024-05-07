with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Raylib; use Raylib;

package Cal is

    procedure DrawCalendar(now : Time; pos: Vector2; fnt: Font);
    function CalendarCellClicked(now: Time; pos: Vector2) return Integer;

private

    type Week_Number is range 0..6;

    fnt_size : Float := 20.0;
    cell_width  : Integer := 33;
    cell_height : Integer := 33;

end Cal;
