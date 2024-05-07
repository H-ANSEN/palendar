with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Raylib; use Raylib;
with RGUI.Component; use RGUI.Component;

package Cal is

    type Calendar_T is new Component_T with record
        fnt   : Font := GetFontDefault;
        ntime : Time := Ada.Calendar.Clock;
    end record;

    overriding procedure Draw(self: in out Calendar_T);
    overriding procedure Update(self: in out Calendar_T);

    --procedure DrawCalendar(now : Time; pos: Vector2; fnt: Font);

private

    type Week_Number is range 0..6;

    fnt_size : Float := 20.0;
    cell_width  : Integer := 33;
    cell_height : Integer := 33;

end Cal;
