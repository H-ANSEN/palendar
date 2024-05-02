with Interfaces.C; use Interfaces.C;
with Raylib; use Raylib;
with Clock; use Clock;

with Ada.Calendar; 

procedure Tide is
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 500;
    screen_center_v : constant Vector2 := (Float(screen_w / 2), Float(screen_h / 2));
begin
    InitWindow(screen_w, screen_h, To_C("tide"));
    SetTargetFPS(60);

    while not WindowShouldClose loop
        BeginDrawing;
            ClearBackground(WHITE);
            DrawClockSmooth(Ada.Calendar.Clock, screen_center_v, 100.0);
        EndDrawing;
    end loop;

    CloseWindow;
end Tide;
