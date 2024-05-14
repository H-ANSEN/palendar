with Interfaces.C; use Interfaces.C;
with Raylib; use Raylib;

with Scene; use Scene;
with Scene.Cal; use Scene.Cal;

with Notepad; use Notepad;

procedure Palendar is
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 400;

    pad : Notepad_T;

    currentScene : CalendarScene; 
begin
    SetConfigFlags(FLAG_WINDOW_RESIZABLE'Enum_Rep);
    InitWindow(screen_w, screen_h, To_C("Palendar"));
    SetWindowMinSize(screen_w, screen_h);
    SetTargetFPS(60);

    pad.fnt := LoadFont("Retron2000.ttf");
    SetTextureFilter(pad.fnt.texture, TEXTURE_FILTER_BILINEAR);

    currentScene.OnLoad;

    while not WindowShouldClose loop
        currentScene.Update;

        BeginDrawing;
            ClearBackground(WHITE);
            --currentScene.Draw;

            pad.Draw;

        EndDrawing;
    end loop;

    UnloadFont(pad.fnt);
    currentScene.OnUnload;
    CloseWindow;
end Palendar;
