with RGUI.Component; use RGUI.Component;

package body RGUI.Layout.Grid is

-- Private ---------------------------------------------------------------------

    function RowPreferredHeight(self: Grid_T; row: Integer) return Float is
        max  : Float := 0.0;
        comp : ComponentRef;
    begin
        for col in 1..self.cols loop
            comp := self.components(row, col);

            if comp /= null then
                max := Float'Max(max, comp.pref_size.y);
            end if;
        end loop;

        return max;
    end;

    function RowPreferredWidth(self: Grid_T; row: Integer) return Float is
        max  : Float := 0.0;
        comp : ComponentRef;
    begin
        for col in 1..self.cols loop
            comp := self.components(row, col);

            if comp /= null then
                max := max + comp.pref_size.x + self.vgap;
            end if;
        end loop;

        return max - self.vgap;
    end;
    
-- Public ----------------------------------------------------------------------

    overriding procedure LayoutComponents(self: Grid_T; bounds: Rectangle) is
        top  : Float := bounds.y + self.inset.top;
        left : Float := bounds.x + self.inset.left;
        
        scale_factor : Float;
        scale        : Float;
        comp         : ComponentRef;
    begin
        for row in 1..self.rows loop
            for col in 1..self.cols loop
                comp := self.components(row, col);

                if comp /= null then
                    comp.x := left;
                    comp.y := top;

                    if bounds.width < comp.pref_size.x then
                        scale_factor := bounds.width / RowPreferredWidth(self, row);
                        scale        := comp.pref_size.x * scale_factor;
                        comp.width   := scale;
                    end if;

                    if bounds.height < comp.pref_size.y then
                        scale_factor := bounds.height / RowPreferredHeight(self, row);
                        scale        := comp.pref_size.y * scale_factor;
                        comp.height  := scale;
                    end if;

                    if row /= 1 then
                        comp.y := comp.y - (comp.pref_size.y - scale);
                    end if;

                    left := left + self.vgap + comp.width;
                end if;
            end loop;
            top  := top + self.hgap + RowPreferredHeight(self, row);
            left := bounds.x + self.inset.left;
        end loop;
    end;
     
    overriding function MinimumSize(self: Grid_T) return Vector2 is
    begin
        return (0.0, 0.0);
    end;

end RGUI.Layout.Grid;
