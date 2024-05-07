with Raylib; use Raylib;

package RGUI.Layout is

    type Insets is record
        top    : Float;
        bottom : Float;
        left   : Float;
        right  : Float;
    end record;

    type Layout is interface;

    procedure LayoutComponents(self: Layout; bounds: Rectangle) is abstract;
    function MinimumSize(self: Layout) return Vector2 is abstract;

end RGUI.Layout;
