with Interfaces.C; use Interfaces.C;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting; 

with Raylib; use Raylib;

package body Clok is

-- Private Math ----------------------------------------------------------------
-- This is so ugly how are you supposed to format big functions in ada

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

    function second_hand_v_smooth(second  : Second_Number;
                                  sub_sec : Second_Duration;
                                  center  : Vector2;
                                  radius  : Float) return Vector2 is

        second_angle   : constant Float := Float(second * 6);
        sub_sec_offset : constant Float := Float(sub_sec) * 6.0;

        second_rad : constant Float   := degree_to_radian(second_angle + sub_sec_offset - 90.0);
        second_v   : constant Vector2 := polar_to_cartesian(radius, second_rad);
    begin
        return (second_v.x + center.x, 
                second_v.y + center.y);
    end;

    function minute_hand_v_smooth(minute : Minute_Number; 
                                  second : Second_Number; 
                                  center : Vector2;
                                  radius : Float) return Vector2 is

        minute_angle  : constant Float := Float(minute * 6);
        second_offset : constant Float := Float(second) * 0.1;

        minute_rad : constant Float   := degree_to_radian(minute_angle + second_offset - 90.0);
        minute_v   : constant Vector2 := polar_to_cartesian(radius, minute_rad);
    begin
        return (minute_v.x + center.x,
                minute_v.y + center.y);
    end;

    function hour_hand_v_smooth(hour   : Hour_Number;
                                minute : Minute_Number;
                                second : Second_Number;
                                center : Vector2;
                                radius : Float) return Vector2 is

        hour_angle    : constant Float := Float((hour mod 12) * 30);
        minute_offset : constant Float := Float(minute) * 0.5;
        second_offset : constant Float := Float(second) * (1.0 / 120.0);

        hour_rad : constant Float   := degree_to_radian(hour_angle + minute_offset + second_offset - 90.0);
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
-- TODO: For 'DrawClock' store vector coords at package level and only update as
-- needed to save some computation. i.e. only update 'minute_v' each minute and 
-- 'hour_v' each hour.

    procedure DrawClock(now: Time; center: Vector2; radius: Float) is
        time_zone   : constant Time_Offset     := UTC_Time_Offset;
        hour_t      : constant Hour_Number     := Hour(now, time_zone);
        minute_t    : constant Minute_Number   := Minute(now, time_zone);
        second_t    : constant Second_Number   := Second(now);

        hour_v   : constant Vector2 := hour_hand_v(hour_t, center, radius - 20.0);
        minute_v : constant Vector2 := minute_hand_v(minute_t, center, radius);
        second_v : constant Vector2 := second_hand_v(second_t, center, radius);
    begin
        DrawClockFace(center, radius);
        DrawLineEx(center, hour_v, 2.0, BLACK);
        DrawLineEx(center, minute_v, 2.0, BLACK);
        DrawLineEx(center, second_v, 2.0, RED);
    end;

    procedure DrawClockSmooth(now: Time; center: Vector2; radius: Float) is
        time_zone   : constant Time_Offset     := UTC_Time_Offset;
        hour_t      : constant Hour_Number     := Hour(now, time_zone);
        minute_t    : constant Minute_Number   := Minute(now, time_zone);
        second_t    : constant Second_Number   := Second(now);
        subsec_t    : constant Second_Duration := Sub_Second(now);

        hour_v   : constant Vector2 := hour_hand_v_smooth(hour_t, minute_t, second_t, center, radius - 20.0);
        minute_v : constant Vector2 := minute_hand_v_smooth(minute_t, second_t, center, radius);
        second_v : constant Vector2 := second_hand_v_smooth(second_t, subsec_t, center, radius);
    begin
        DrawClockFace(center, radius);
        DrawLineEx(center, hour_v, 2.0, BLACK);
        DrawLineEx(center, minute_v, 2.0, BLACK);
        DrawLineEx(center, second_v, 2.0, RED);
    end;

end Clok;
