with Interfaces.C; use Interfaces.C;
with Raylib; use Raylib;

with Scene; use Scene;
with Scene.Cal; use Scene.Cal;

procedure Palendar is
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 400;

    currentScene : CalendarScene; 
begin
    SetConfigFlags(FLAG_WINDOW_RESIZABLE'Enum_Rep);
    InitWindow(screen_w, screen_h, To_C("Palendar"));
    SetWindowMinSize(screen_w, screen_h);
    SetTargetFPS(30);

    currentScene.OnLoad;

    while not WindowShouldClose loop
        currentScene.Update;

        BeginDrawing;
            ClearBackground(WHITE);
            currentScene.Draw;
        EndDrawing;
    end loop;

    currentScene.OnUnload;
    CloseWindow;
end Palendar;
