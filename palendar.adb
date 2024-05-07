with Interfaces.C; use Interfaces.C;
with Ada.Calendar; 
with Ada.Text_IO;

with Raylib; use Raylib;
with Clock; use Clock;
with Cal; use Cal;

procedure Palendar is
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 400;
    screen_center_v : constant Vector2 := (Float(screen_w / 2), Float(screen_h / 2));

    clock_pos : constant Vector2 := (screen_center_v.x - 230.0, screen_center_v.y - 100.0);
    calendar_pos : constant Vector2 := (screen_center_v.x + 30.0, screen_center_v.y - 100.0);

    cal  : Calendar_T; 
begin
    InitWindow(screen_w, screen_h, To_C("tide"));
    SetTargetFPS(60);

    cal := CalendarMake(calendar_pos);

    while not WindowShouldClose loop
        BeginDrawing;
            ClearBackground(WHITE);
            DrawClock(Ada.Calendar.Clock, clock_pos, 100.0);
            cal.Draw;
        EndDrawing;
    end loop;

    CalendarDestory(cal);
    CloseWindow;
end Palendar;
