with RGUI.Component; use RGUI.Component;

package body RGUI.Layout.Grid is

-- Private ---------------------------------------------------------------------

    function RowMinimumSize(self: Grid_T; row: Integer) return Vector2 is
        width  : Float := 0.0;
        height : Float := 0.0;
        comp   : ComponentRef;
    begin
        for col in 1..self.cols loop
            comp := self.components(row, col);
            if comp /= null then
                width  := width + comp.min_size.x + self.vgap;
                height := Float'Max(height, comp.min_size.y);
            end if;
        end loop;

        return (width - self.vgap, height);
    end;

    function GridMinimumSize(self: Grid_T) return Vector2 is
        width  : Float := 0.0;
        height : Float := 0.0;
        size   : Vector2;
    begin
        for row in 1..self.rows loop
            size   := RowMinimumSize(self, row); 
            width  := Float'Max(width, size.x);
            height := height + size.y + self.hgap;
        end loop;

        width := width + self.inset.left + self.inset.right;
        height := height + self.inset.top + self.inset.bottom;

        return (width, height - self.hgap);
    end;
    
-- Public ----------------------------------------------------------------------

    overriding procedure LayoutComponents(self: Grid_T; bounds: Rectangle) is
        top  : Float   := bounds.y + self.inset.top;
        left : Float   := bounds.x + self.inset.left;
        min  : Vector2 := GridMinimumSize(self);

        scale_factor : Float;
        scale        : Float;
        max_height   : Float := 0.0;
        comp         : ComponentRef;
    begin
        for row in 1..self.rows loop 
            left := bounds.x + self.inset.left;
            for col in 1..self.cols loop
                comp := self.components(row, col);
                if comp /= null then

                    comp.x := left;
                    comp.y := top;

                    scale_factor := bounds.width / min.x;
                    scale        := comp.min_size.x * scale_factor;
                    comp.width   := Float'Max(scale, comp.min_size.x);

                    if col /= 1 then
                        comp.x   := comp.x + (self.vgap * scale_factor);
                    end if;

                    scale_factor := bounds.height / min.y;
                    scale        := comp.min_size.y * scale_factor;
                    comp.height  := Float'Max(scale, comp.min_size.y);

                    if row /= 1 then
                        comp.y   := comp.y + (self.hgap * scale_factor);
                    end if;

                    max_height   := Float'Max(max_height, comp.height);
                    left         := left + comp.width;

                end if;
            end loop;
            top := top + max_height;
            max_height := 0.0;
        end loop;
    end;
     
    overriding function MinimumSize(self: Grid_T) return Vector2 is
    begin
        return GridMinimumSize(self);
    end;

end RGUI.Layout.Grid;
