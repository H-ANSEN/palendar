with Raylib; use Raylib;

package body RGUI.Layout.Border is

    overriding procedure LayoutComponents(self: Border_T; bounds: Rectangle) is
        top    : Float := bounds.y + self.inset.top;
        left   : Float := bounds.x + self.inset.left;
        bottom : Float := bounds.height - self.inset.bottom;
        right  : Float := bounds.width - self.inset.right;
    begin
        if self.top /= null then
            self.top.x      := left;
            self.top.y      := top;
            self.top.width  := right - self.inset.left;
            self.top.height := self.top.pref_size.y;

            top := top + self.top.pref_size.y + self.vgap;
        end if;

        if self.bottom /= null then
            self.bottom.x      := left;
            self.bottom.y      := bottom - self.bottom.pref_size.y;
            self.bottom.width  := right - self.inset.left;
            self.bottom.height := self.bottom.pref_size.y + bounds.y;

            bottom := bottom - self.bottom.pref_size.y - self.vgap;
        end if;

        if self.right /= null then
            self.right.x      := right - self.right.pref_size.x;
            self.right.y      := top;
            self.right.width  := self.right.pref_size.x + bounds.x;
            self.right.height := bottom - top;

            right := right - self.right.pref_size.x - self.hgap;
        end if;

        if self.left /= null then
            self.left.x      := left;
            self.left.y      := top;
            self.left.width  := self.left.pref_size.x;
            self.left.height := bottom - top;

            left := left + self.left.pref_size.x + self.hgap;
        end if;

        if self.center /= null then
            self.center.x      := left;
            self.center.y      := top;
            self.center.width  := right - left;
            self.center.height := bottom - top;
        end if;
    end;

    overriding function MinimumSize(self: Border_T) return Vector2 is
        min : Vector2 := (0.0, 0.0);
    begin
        if self.right /= null then
            min.x := min.x + self.right.min_size.x + self.hgap;
            min.y := Float'Max(min.y, self.right.min_size.y);
        end if;

        if self.left /= null then
            min.x := min.x + self.left.min_size.x + self.hgap;
            min.y := Float'Max(min.y, self.left.min_size.y);
        end if;
        
        if self.center /= null then
            min.x := min.x + self.center.min_size.x;
            min.y := Float'Max(min.y, self.center.min_size.y);
        end if;

        if self.top /= null then
            min.x := Float'Max(min.x, self.top.min_size.x);
            min.y := min.y + self.top.min_size.y + self.vgap;
        end if;

        if self.bottom /= null then
            min.x := Float'Max(min.x, self.bottom.min_size.x);
            min.y := min.y + self.bottom.min_size.y + self.vgap;
        end if;

        min.x := min.x + self.inset.left + self.inset.right;
        min.y := min.y + self.inset.top + self.inset.bottom;

        return min;

    end;

end RGUI.Layout.Border;
