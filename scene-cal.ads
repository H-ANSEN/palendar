with Clock; use Clock;
with Cal; use Cal;

with RGUI.Layout.Grid; use RGUI.Layout.Grid;
with RGUI.Component.Rec; use RGUI.Component.Rec;

package Scene.Cal is

    type CalendarScene is new Scene_I with record
        glayout       : Grid_T (1, 2);
        clock_comp    : aliased Clock_T;
        calendar_comp : aliased Calendar_T;
        bounds_rec    : Rec_T;
    end record;

    overriding procedure OnLoad(self: out CalendarScene);
    overriding procedure OnUnload(self: CalendarScene);
    overriding procedure Draw(self: in out CalendarScene);
    overriding procedure Update(self: in out CalendarScene); 

end Scene.Cal;
