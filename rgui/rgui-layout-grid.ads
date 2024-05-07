with Raylib; use Raylib;
with RGUI.Component; use RGUI.Component;

package RGUI.Layout.Grid is

    type ComponentArr is array (Positive range <>, Positive range <>) of ComponentRef;

    type Grid_T (rows, cols : Positive) is new Layout with record
        components : ComponentArr (1..rows, 1..cols);
        vgap       : Float  := 0.0;
        hgap       : Float  := 0.0;
        inset      : Insets := (0.0, 0.0, 0.0, 0.0);
    end record;

    overriding procedure LayoutComponents(self: Grid_T; bounds: Rectangle);
    overriding function MinimumSize(self: Grid_T) return Vector2;

end RGUI.Layout.Grid;
