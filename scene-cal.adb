with Raylib;

with RGUI.Component; use RGUI.Component;
with Clock; use Clock;
with Cal; use Cal;

package body Scene.Cal is

    overriding procedure OnLoad(self: out CalendarScene) is
    begin
        self.glayout := (self.glayout with delta
            vgap   => 55.0, 
            inset  => (90.0, 50.0, 50.0, 50.0)
        );

        self.bounds_rec := (
            pref_size => (100.0, 100.0),
            min_size  => (50.0, 50.0),
            width     => Float(Raylib.GetScreenWidth),
            height    => Float(Raylib.GetScreenHeight),
            bg_color  => (255, 250, 255, 255),
            others    => <>
        );

        self.clock_comp := (
            pref_size => (200.0, 200.0),
            min_size  => (200.0, 200.0),
            bg_color  => (0, 255, 0, 150),
            others => <>
        );

        self.calendar_comp := (
            pref_size => (200.0, 200.0),
            min_size  => (200.0, 200.0),
            bg_color  => (0, 255, 0, 150),
            others => <>
        );

        self.glayout.components(1, 1) := Component_T(self.clock_comp)'Unchecked_Access;
        self.glayout.components(1, 2) := Component_T(self.calendar_comp)'Unchecked_Access;
        self.bounds_rec.min_size      := self.glayout.MinimumSize;
        self.glayout.LayoutComponents(self.bounds_rec.GetBounds);

        -- TODO for now load font here and unload on scene unload should be loaded and
        -- unloaded with with the rest of program initialization
        self.calendar_comp.fnt := Raylib.LoadFont("Retron2000.ttf");
        Raylib.SetTextureFilter(self.calendar_comp.fnt.texture, Raylib.TEXTURE_FILTER_BILINEAR);
    end;

    overriding procedure OnUnload(self: CalendarScene) is
    begin 
        Raylib.UnloadFont(self.calendar_comp.fnt);
    end;

    overriding procedure Draw(self: in out CalendarScene) is
    begin
        self.bounds_rec.Draw;
        self.clock_comp.Draw;
        self.calendar_comp.Draw;
    end;

    overriding procedure Update(self: in out CalendarScene) is
    begin
        if Raylib.IsWindowResized then
            self.bounds_rec.width  := Float'Max(Float(Raylib.GetScreenWidth), self.bounds_rec.min_size.x);
            self.bounds_rec.height := Float'Max(Float(Raylib.GetScreenHeight), self.bounds_rec.min_size.y);
            self.glayout.LayoutComponents(self.bounds_rec.GetBounds);
        end if;
        
        self.clock_comp.Update;
        self.calendar_comp.Update;
    end;

end Scene.Cal;
