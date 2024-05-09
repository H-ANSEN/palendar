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

    bounds : Rectangle := (0.0, 0.0, 0.0, 0.0);

    child_rec : ComponentRef := new Rec_T'(
        pref_size => (100.0, 100.0),
        min_size  => (50.0, 50.0),
        bg_color  => (255, 255, 255, 255),
        others    => <>
    );

    blue_rec : ComponentRef := new Rec_T'(
        pref_size => (50.0, 50.0),
        min_size  => (50.0, 50.0),
        bg_color  => (0, 0, 255, 150),
        others    => <>
    );

    black_rec : ComponentRef := new Rec_T'(
        pref_size => (50.0, 50.0),
        min_size  => (50.0, 50.0),
        bg_color  => (0, 0, 0, 150),
        others    => <>
    );

    red_rec : ComponentRef := new Rec_T'(
        pref_size => (50.0, 50.0),
        min_size  => (50.0, 50.0),
        bg_color  => (255, 0, 0, 150),
        others    => <>
    );

    green_rec : ComponentRef := new Rec_T'(
        pref_size => (50.0, 50.0),
        min_size  => (50.0, 50.0),
        bg_color  => (0, 255, 0, 150),
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

    border_layout : Border_T := (
        left   => blue_rec,
        right  => red_rec,
        top    => green_rec,
        bottom => black_rec,
        center => child_rec,
        others => <>
    );

    grid_layout : Grid_T := (
        rows   => 1,
        cols   => 2,
        vgap   => 10.0,
        others => <>
    );

    mousePos : Vector2;
    minSize  : Vector2; 
    screen_w : constant Integer := 600;
    screen_h : constant Integer := 400;
begin
    grid_layout.components(1, 1) := clock_comp;
    grid_layout.components(1, 2) := calendar_comp;

    child_rec.min_size := grid_layout.MinimumSize;
    minSize := border_layout.MinimumSize;
    
    InitWindow(screen_w, screen_h, To_C("Palendar"));
    SetTargetFPS(60);

    Calendar_T(calendar_comp.all).fnt := LoadFont("Retron2000.ttf");
    SetTextureFilter(Calendar_T(calendar_comp.all).fnt.texture, TEXTURE_FILTER_BILINEAR);

    while not WindowShouldClose loop
        mousePos := GetMousePosition;

        if mousePos.x < minSize.x then 
            bounds.width := minSize.x;
        else 
            bounds.width := mousePos.x;
        end if;

        if mousePos.y < minSize.y then 
            bounds.height := minSize.y;
        else 
            bounds.height := mousePos.y;
        end if;

        border_layout.LayoutComponents(bounds);
        grid_layout.LayoutComponents(child_rec.GetBounds);

        clock_comp.Update;
        calendar_comp.Update;

        BeginDrawing;
            ClearBackground(WHITE);

            child_rec.Draw;
            red_rec.Draw;
            blue_rec.Draw;
            green_rec.Draw;
            black_rec.Draw;
            clock_comp.Draw;
            calendar_comp.Draw;
        EndDrawing;
    end loop;

    UnloadFont(Calendar_T(calendar_comp.all).fnt);
    CloseWindow;
end Palendar;
