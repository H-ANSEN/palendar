with Interfaces.C; use Interfaces.C;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting; 

with Raylib; use Raylib;

package body Clock is

-- Private Math ----------------------------------------------------------------

    function degree_to_radian(degree: Float) return Float is
    begin
        return degree * (Pi / 180.0);
    end;

    function polar_to_cartesian(radius, rad: Float) return Vector2 is 
    begin
        return (radius * Cos(rad), radius * Sin(rad));
    end;

    function second_hand_v(second : Second_Number; 
                           center : Vector2; 
                           radius : Float) return Vector2 is

        second_rad : constant Float   := degree_to_radian(Float((second * 6) - 90));
        second_v   : constant Vector2 := polar_to_cartesian(radius, second_rad);
    begin
        return (second_v.x + center.x, 
                second_v.y + center.y);
    end;

    function minute_hand_v(minute : Minute_Number; 
                           center : Vector2; 
                           radius : Float) return Vector2 is

        minute_rad : constant Float   := degree_to_radian(Float((minute * 6) - 90));
        minute_v   : constant Vector2 := polar_to_cartesian(radius, minute_rad);
    begin
        return (minute_v.x + center.x,
                minute_v.y + center.y);
    end;

    function hour_hand_v(hour: Hour_Number; 
                         center: Vector2; 
                         radius: Float) return Vector2 is

        hour_rad : constant Float   := degree_to_radian((Float(hour mod 12) * 30.0) - 90.0);
        hour_v   : constant Vector2 := polar_to_cartesian(radius, hour_rad);
    begin
        return (hour_v.x + center.x,
                hour_v.y + center.y);
    end;

-- Private Drawing -------------------------------------------------------------

    -- TODO: This is placeholder clock face should implement something a bit 
    -- prettier
    --
    -- Future idea could be allowing user to add image as clock face. Maybe even
    -- make a small canvas where they can draw a clock face??
    procedure DrawClockFace(center: Vector2; radius: Float) is
        coords : Vector2;
    begin
        for i in Second_Number'Range loop
            coords := second_hand_v(i, center, radius);

            if i mod 5 = 0 then DrawText(To_C("*"), Integer(coords.x), Integer(coords.y), 8, BLACK);
            else DrawPixelV(second_hand_v(i, center, radius), BLACK); end if;
        end loop;
    end;

-- Public ----------------------------------------------------------------------

    overriding procedure Draw(self: in out Clock_T) is
        time_zone : Time_Offset   := UTC_Time_Offset;
        hour_t    : Hour_Number   := Hour(self.ntime, time_zone);
        minute_t  : Minute_Number := Minute(self.ntime, time_zone);
        second_t  : Second_Number := Second(self.ntime);

        radius   : Float   := Float'Min(self.width, self.height) / 2.0;
        center   : Vector2 := (self.x + radius, self.y + radius);
        hour_v   : Vector2 := hour_hand_v(hour_t, center, radius * 0.8);
        minute_v : Vector2 := minute_hand_v(minute_t, center, radius);
        second_v : Vector2 := second_hand_v(second_t, center, radius);
    begin
        DrawClockFace(center, radius);
        DrawLineEx(center, hour_v, 3.0, BLACK);
        DrawLineEx(center, minute_v, 3.0, BLACK);
        DrawLineEx(center, second_v, 3.0, RED);
    end;

    overriding procedure Update(self: in out Clock_T) is
    begin
        self.ntime := Ada.Calendar.Clock;
    end;

end Clock;
