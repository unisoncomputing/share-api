
CREATE OR REPLACE FUNCTION create_cursor(cursor_name text)
RETURNS refcursor AS $$
DECLARE
    segments text[];
    input_segments text[];
    current_segment text;
BEGIN
    input_segments := string_to_array(trim(trailing '.' from fqn), '.');
    segments := ARRAY[]::text[];

    FOREACH current_segment IN ARRAY input_segments
    LOOP
        segments := array_append(segments, array_to_string(input_segments[1:array_position(input_segments, current_segment)], '.') || '.');
    END LOOP;

    RETURN segments;
END;
$$ LANGUAGE plpgsql;
