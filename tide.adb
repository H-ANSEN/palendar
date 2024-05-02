with Interfaces.C; use Interfaces.C;
with Raylib; use Raylib;
with Clok; use Clok;

with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

procedure Tide is
    year      : Year_Number;
    month     : Month_Number;
    day       : Day_Number;
    hour      : Hour_Number;
    minute    : Minute_Number;
    second    : Second_Number;
    sub_sec   : Second_Duration;
    time_zone : Time_Offset := UTC_Time_Offset;

    second_v : Vector2;
    minute_v : Vector2;
    hour_v   : Vector2;

    screen_w : constant Integer := 300;
    screen_h : constant Integer := 500;
    screen_wh: constant Integer := screen_w / 2;
    screen_hh: constant Integer := screen_h / 2;
    screen_v : constant Vector2 := (Float(screen_wh), Float(screen_hh));

    camera : Camera2D := ((0.0, 0.0), (0.0, 0.0), 0.0, 1.0);

    RED   : constant Color := (255,   0,   0, 255);
    WHITE : constant Color := (255, 255, 255, 255);
    BLACK : constant Color := (  0,   0,   0, 255);
begin
    InitWindow(screen_w, screen_h, To_C("tide"));
    SetTargetFPS(60);

    while not WindowShouldClose loop
        Split(Clock, year, month, day, hour, minute, second, sub_sec, time_zone);

        second_v := second_hand_coords(second, screen_wh, screen_hh, 100);
        minute_v := minute_hand_coords(minute, second, screen_wh, screen_hh, 100);
        hour_v := hour_hand_coords(hour, minute, second, screen_wh, screen_hh, 80);

        BeginDrawing;
            ClearBackground(WHITE);
            BeginMode2D(camera);

                for i in Second_Number'Range loop
                    DrawPixelV(second_hand_coords(i, screen_wh, screen_hh, 100), BLACK);
                end loop;

                DrawText(To_C(Year_Number'Image(second)), 20, 20, 30, BLACK);
                DrawLineEx(screen_v, minute_v, 2.0, BLACK);
                DrawLineEx(screen_v, hour_v, 2.0, BLACK);
                DrawLineEx(screen_v, second_v, 2.0, RED);
            EndMode2D;
        EndDrawing;
    end loop;

    CloseWindow;
end Tide;
