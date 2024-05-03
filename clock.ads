with Ada.Calendar; use Ada.Calendar;
with Raylib; use Raylib;

package Clock is

    procedure DrawClock(now: Time; pos: Vector2; radius: Float);
    procedure DrawClockSmooth(now: Time; pos: Vector2; radius: Float);

end Clock;

