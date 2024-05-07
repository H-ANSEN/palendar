with Raylib; use Raylib;

package body RGUI.Component is

    procedure SetBounds(self: out Component_T; bounds: Rectangle) is
    begin
        self.x      := bounds.x;
        self.y      := bounds.y;
        self.width  := bounds.width;
        self.height := bounds.height;
    end;

end RGUI.Component;
