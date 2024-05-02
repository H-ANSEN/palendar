with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Raylib; use Raylib;

package body Clok is

    function degree_to_rad(degree: Float) return Float is
    begin
        return degree * (Pi / 180.0);
    end;

-- Public ----------------------------------------------------------------------

    function second_hand_coords(second: Second_Number; 
                                centerX, centerY, radius: Integer) return Vector2 is
        second_rad : Float;
        second_x   : Float;
        second_y   : Float;
    begin 
        second_rad := degree_to_rad(Float(second * 6) - 90.0);
        second_x := Float(radius) * Cos(second_rad);
        second_y := Float(radius) * Sin(second_rad);
        return (second_x + Float(centerX), second_y + Float(centerY));
    end;

    function minute_hand_coords(minute: Minute_Number; 
                                second: Second_Number; 
                                centerX, centerY, radius: Integer) return Vector2 is
        minute_rad : Float;
        minute_x   : Float;
        minute_y   : Float;
    begin
        minute_rad := degree_to_rad((Float(minute) * 6.0 + Float(second) * 0.1) - 90.0);
        minute_x := Float(radius) * Cos(minute_rad);
        minute_y := Float(radius) * Sin(minute_rad);
        return (minute_x + Float(centerX), minute_y + Float(centerY));
    end;

    function hour_hand_coords(hour: Hour_Number;
                              minute: Minute_Number;
                              second: Second_Number;
                              centerX, centerY, radius: Integer) return Vector2 is
        hour_rad : Float;
        hour_x   : Float;
        hour_y   : Float;
    begin
        hour_rad := degree_to_rad(Float((hour mod 12) * 30 + minute) * 0.5 + Float(second) * (1.0/120.0));
        hour_x := Float(radius) * Cos(hour_rad);
        hour_y := Float(radius) * Sin(hour_rad);
        return (hour_x + Float(centerX), hour_y + Float(centerY));
    end;

end Clok;
