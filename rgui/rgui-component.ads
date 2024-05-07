with Raylib; use Raylib;

package RGUI.Component is

-- Abstract Component Def ------------------------------------------------------

    type Component_T is abstract tagged record
        x, y          : Float;
        width, height : Float;
        pref_size     : Vector2;
        min_size      : Vector2;
        bg_color      : Color;
    end record;

    procedure Draw(self: Component_T) is abstract;
    procedure Update(self: in out Component_T) is abstract;

-- Provided Def ----------------------------------------------------------------

    procedure SetBounds(self: out Component_T; bounds: Rectangle);

-- Types -----------------------------------------------------------------------

    type ComponentRef is access all Component_T'Class;

end RGUI.Component;
