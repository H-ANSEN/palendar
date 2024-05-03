with Interfaces.C; use Interfaces.C;
with Ada.Calendar; 

with Raylib; use Raylib;
with Clock; use Clock;
with Cal; use Cal;

procedure Tide is
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 400;
    screen_center_v : constant Vector2 := (Float(screen_w / 2), Float(screen_h / 2));

    clock_pos : constant Vector2 := (screen_center_v.x - 230.0, screen_center_v.y - 100.0);
    calendar_pos : constant Vector2 := (screen_center_v.x + 30.0, screen_center_v.y - 100.0);

    fnt    : Font;
begin
    InitWindow(screen_w, screen_h, To_C("tide"));
    SetTargetFPS(60);

    fnt := LoadFont(To_C("Retron2000.ttf"));
    SetTextureFilter(fnt.texture, TEXTURE_FILTER_POINT);

    while not WindowShouldClose loop
        BeginDrawing;
            ClearBackground(WHITE);
            DrawClock(Ada.Calendar.Clock, clock_pos, 100.0);
            DrawCalendar(Ada.Calendar.Clock, calendar_pos, fnt);
        EndDrawing;
    end loop;

    UnloadFont(fnt);
    CloseWindow;
end Tide;
