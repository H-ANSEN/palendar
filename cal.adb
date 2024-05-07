with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting; 

with Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Interfaces.C; use Interfaces.C; 
with Raylib; use Raylib;

package body Cal is

-- Private Helper --------------------------------------------------------------

    function is_leap_year(year: Year_Number) return Boolean is
    begin
        return ((year mod 4)   = 0 and 
                (year mod 100) = 0 and
                (year mod 400) = 0
        );
    end;

    function days_in_month(year: Year_Number; month: Month_Number) return Integer is
    begin
        return (
            case month is
                when 1  => 31,
                when 2  => (if is_leap_year(year) then 29 else 28),
                when 3  => 31,
                when 4  => 30,
                when 5  => 31,
                when 6  => 30,
                when 7  => 31,
                when 8  => 31,
                when 9  => 30,
                when 10 => 31,
                when 11 => 30,
                when 12 => 31
        );
    end;

    function week_num_of(year : Year_Number; month : Month_Number; 
                         day  : Day_Number) return Week_Number 
    is
        y : Integer := Integer(year);
        m : Integer := Integer(month);
        d : Integer := Integer(day);
        c : Integer;
    begin
        if m < 3 then
            y := y - 1;
            m := m + 10;
        else
            m := m - 2;
        end if;

        c := y / 100;
        y := y mod 100;
        return Week_Number(((26 * m - 2)/10 + d + y + y/4 + c/4 - 2*c) mod 7);
    end;

    function day_name_of(num: Week_Number) return Day_Name is
    begin
        return (
            case num is
                when 0 => Sunday,
                when 1 => Monday,
                when 2 => Tuesday,
                when 3 => Wednesday,
                when 4 => Thursday,
                when 5 => Friday,
                when 6 => Saturday
        );
    end;

-- Private Drawing -------------------------------------------------------------

    procedure DrawCenteredText(text: String; size: Float; fnt: Font; col: Color; rec: Rectangle) is
        text_size     : Vector2;
        text_draw_pos : Vector2;
    begin
        text_size := MeasureTextEx(fnt, To_C(Trim(text, Both)), size, 1.0);
        text_draw_pos.x := rec.x + (rec.width - text_size.x) / 2.0;
        text_draw_pos.y := rec.y + (rec.height - text_size.y) / 2.0;

        DrawTextEx(fnt, To_C(Trim(text, Both)), text_draw_pos, size, 1.0, col);
    end;

    procedure DrawCalendarBackground(pos: Vector2; rows: Integer) is
        cell_w : Float := Float(cell_width);
        cell_h : Float := Float(cell_height);
    begin
        DrawRectangleRec((pos.x, pos.y, cell_w, cell_h), RED);
        DrawRectangleRec((pos.x + cell_w, pos.y, cell_w * 5.0, cell_h), GRAY);
        DrawRectangleRec((pos.x + cell_w * 6.0, pos.y, cell_w, cell_h), BLUE);

        DrawRectangleRec((pos.x, pos.y + cell_h, cell_w, cell_h * Float(rows)), PINK);
        DrawRectangleRec((pos.x + cell_w * 6.0, pos.y + cell_h, cell_w, cell_h * Float(rows)), LIGHTBLUE);
    end;

    procedure DrawCalendarHeader(pos: Vector2; fnt: Font) is
        day_text      : String (1..2);
        cell_x        : Integer;
        cell          : Rectangle;
    begin
        for i in Week_Number'Range loop
            day_text      := day_name_of(i)'Image(1..2);
            cell_x        := Integer(pos.x) + Integer(i) * cell_width;
            cell := (Float(cell_x), pos.y, Float(cell_width), Float(cell_height));

            DrawCenteredText(day_text, fnt_size, fnt, WHITE, cell);
        end loop;
    end;

    procedure DrawCalendarNumbers(pos: Vector2; fnt: Font; year, month, start_day: Integer) is
        cell_x : Float;
        cell_y : Float;
        cell   : Rectangle;
    begin
        for i in start_day..(days_in_month(year, month) + start_day - 1) loop

            cell_x := pos.x + Float(cell_width * (i mod 7));
            cell_y := pos.y + Float(cell_height * (i / 7)) + Float(cell_height);
            cell   := (cell_x, cell_y, Float(cell_width), Float(cell_height));

            DrawCenteredText(Integer'Image(i - start_day + 1), fnt_size, fnt, BLACK, cell);
        end loop;
    end;

    procedure DrawCalendarGrid(pos: Vector2; rows: Integer) is
        cell_w : Float := Float(cell_width);
        cell_h : Float := Float(cell_height);
        calendar_w : Float := cell_w * 7.0;
        calendar_h : Float := cell_h * (Float(rows) + 1.0);
    begin
        for i in 2..rows loop
            DrawLineEx((pos.x, pos.y + cell_h * Float(i)), (pos.x + calendar_w, pos.y + cell_h * Float(i)), 2.0, LIGHTGREY); 
        end loop;

        DrawLineEx((pos.x + cell_w, pos.y + cell_h), (pos.x + cell_w, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 2.0, pos.y + cell_h), (pos.x + cell_w * 2.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 3.0, pos.y + cell_h), (pos.x + cell_w * 3.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 4.0, pos.y + cell_h), (pos.x + cell_w * 4.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 5.0, pos.y + cell_h), (pos.x + cell_w * 5.0, pos.y + calendar_h), 2.0, LIGHTGREY);
        DrawLineEx((pos.x + cell_w * 6.0, pos.y + cell_h), (pos.x + cell_w * 6.0, pos.y + calendar_h), 2.0, LIGHTGREY);

        DrawLineEx((pos.x, pos.y + cell_h), (pos.x + calendar_w, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w, pos.y), (pos.x + cell_w, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 2.0, pos.y), (pos.x + cell_w * 2.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 3.0, pos.y), (pos.x + cell_w * 3.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 4.0, pos.y), (pos.x + cell_w * 4.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 5.0, pos.y), (pos.x + cell_w * 5.0, pos.y + cell_h), 2.0, BLACK);
        DrawLineEx((pos.x + cell_w * 6.0, pos.y), (pos.x + cell_w * 6.0, pos.y + cell_h), 2.0, BLACK);

        DrawRectangleLinesEx((pos.x, pos.y, Float(calendar_w), Float(calendar_h)), 2.0, BLACK);
    end;

    procedure DrawDayHighlight(pos: Vector2; day, start_day: Integer) is
        cell_x : Float;
        cell_y : Float;
    begin
        cell_x := pos.x + Float(cell_width * ((day + start_day - 1) mod 7));
        cell_y := pos.y + Float(cell_height * ((day + start_day) / 7)) + Float(cell_height);
        DrawRectangleRec((cell_x, cell_y, Float(cell_width), Float(cell_height)), (0,255,0, 150));
    end;

-- Public ----------------------------------------------------------------------

    procedure DrawCalendar(now: Time; pos: Vector2; fnt: Font) is
        year  : Year_Number  := Integer(Ada.Calendar.Year(now));
        month : Month_Number := Integer(Ada.Calendar.Month(now));
        day   : Day_Number   := Integer(Ada.Calendar.Day(now));

        month_start  : Integer; -- 0..6 day of the week the month starts on
        cells_needed : Integer; -- cells in the calendar needed to draw all days
        rows_needed  : Integer; -- rows needed to fit cells in columns of '7'
    begin
        month_start  := Integer(week_num_of(year, month, 1));
        cells_needed := month_start + days_in_month(year, month);
        rows_needed  := Integer(Float'Ceiling(Float(cells_needed) / 7.0));

        DrawCalendarBackground(pos, rows_needed);
        DrawCalendarHeader(pos, fnt);
        DrawCalendarNumbers(pos, fnt, year, month, month_start);
        DrawDayHighlight(pos, day, month_start);
        DrawCalendarGrid(pos, rows_needed);
    end;

    function CalendarCellClicked(now: Time; pos: Vector2) return Integer is
        cal_w : Float := Float(cell_width * 7);
        cal_h : Float := Float(cell_height * 6);
        cal_bounds : Rectangle := (pos.x, pos.y + Float(cell_height), cal_w, cal_h);

        year  : Year_Number  := Integer(Ada.Calendar.Year(now));
        month : Month_Number := Integer(Ada.Calendar.Month(now));
        month_start : Integer;

        mousePos : Vector2 := GetMousePosition;

        cell_x : Integer;
        cell_y : Integer;
    begin
        if CheckCollisionPointRec(mousePos, cal_bounds) and
           IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then

            cell_y := Integer(mousePos.y - (pos.y + Float(cell_height))) / cell_height;
            cell_x := Integer(mousePos.x - pos.x) / cell_width;
            month_start  := Integer(week_num_of(year, month, 1));


            return cell_y * 7 + cell_x - month_start + 1;
        end if;

        return 0;
    end;

end Cal;





