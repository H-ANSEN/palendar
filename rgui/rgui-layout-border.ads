with Raylib; use Raylib;
with RGUI.Component; use RGUI.Component;

package RGUI.Layout.Border is

    type BorderConstraint is (
        TOP,
        BOTTOM,
        LEFT,
        RIGHT,
        CENTER
    );

    type Border_T is new Layout with record
        top    : ComponentRef := null;
        bottom : ComponentRef := null;
        left   : ComponentRef := null;
        right  : ComponentRef := null;
        center : ComponentRef := null;
        vgap   : Float  := 0.0;
        hgap   : Float  := 0.0;
        inset  : Insets := (0.0, 0.0, 0.0, 0.0);
    end record;

    overriding procedure LayoutComponents(self: Border_T; bounds: Rectangle);
    overriding function MinimumSize(self: Border_T) return Vector2;

end RGUI.Layout.Border;
