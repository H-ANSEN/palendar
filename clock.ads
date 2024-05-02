with Ada.Calendar; use Ada.Calendar;
with Raylib; use Raylib;

package Clock is

    procedure DrawClock(now: Time; center: Vector2; radius: Float);
    procedure DrawClockSmooth(now: Time; center: Vector2; radius: Float);

end Clock;

