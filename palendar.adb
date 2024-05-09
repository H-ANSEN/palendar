with Interfaces.C; use Interfaces.C;
with Ada.Calendar; 

with Raylib; use Raylib;
with Clock; use Clock;
with Cal; use Cal;

with RGUI.Component; use RGUI.Component;
with RGUI.Component.Rec; use RGUI.Component.Rec;
with RGUI.Layout.Border; use RGUI.Layout.Border;
with RGUI.Layout.Grid; use RGUI.Layout.Grid;

procedure Palendar is

    window_rec : ComponentRef := new Rec_T'(
        pref_size => (100.0, 100.0),
        min_size  => (50.0, 50.0),
        bg_color  => (255, 250, 255, 255),
        others    => <>
    );

    clock_comp : ComponentRef := new Clock_T'(
        pref_size => (200.0, 200.0),
        min_size  => (200.0, 200.0),
        bg_color  => (0, 255, 0, 150),
        others => <>
    );

    calendar_comp : ComponentRef := new Calendar_T'(
        pref_size => (200.0, 200.0),
        min_size  => (200.0, 200.0),
        bg_color  => (0, 255, 0, 150),
        others => <>
    );

    grid_layout : Grid_T := (
        rows   => 1,
        cols   => 2,
        vgap   => 50.0,
        inset  => (90.0, 50.0, 50.0, 50.0),
        others => <>
    );

    mousePos : Vector2;
    minSize  : Vector2; 
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 400;
begin
    grid_layout.components(1, 1) := clock_comp;
    grid_layout.components(1, 2) := calendar_comp;
    window_rec.min_size := grid_layout.MinimumSize;
    
    InitWindow(screen_w, screen_h, To_C("Palendar"));
    SetTargetFPS(60);

    Calendar_T(calendar_comp.all).fnt := LoadFont("Retron2000.ttf");
    SetTextureFilter(Calendar_T(calendar_comp.all).fnt.texture, TEXTURE_FILTER_BILINEAR);

    while not WindowShouldClose loop
        mousePos := GetMousePosition;
        window_rec.width  := Float'Max(mousePos.x, window_rec.min_size.x);
        window_rec.height := Float'Max(mousePos.y, window_rec.min_size.y);

        grid_layout.LayoutComponents(window_rec.GetBounds);

        clock_comp.Update;
        calendar_comp.Update;

        BeginDrawing;
            ClearBackground(WHITE);

            window_rec.Draw;
            clock_comp.Draw;
            calendar_comp.Draw;
        EndDrawing;
    end loop;

    UnloadFont(Calendar_T(calendar_comp.all).fnt);
    CloseWindow;
end Palendar;
