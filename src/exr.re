let magic_number: int32 = 20000630l;
let version: int32 = 2l;
let pixel_type_float: int32 = 2l;
let compression_none: char = char_of_int(0);
let line_order_increasing_y: char = char_of_int(0);

type exr_data = {
    buffer: Buffer.t,
    width: int,
    height: int,
    data_offset: int,
};

let char_of_int32 = (i: int32) => {
    char_of_int(Int32.to_int(i) land 0xff)
};

let write_little_endian = (buffer: Buffer.t, i: int32) => {
    Buffer.add_char(buffer, char_of_int32(i));
    Buffer.add_char(buffer, char_of_int32(Int32.shift_right(i, 8)));
    Buffer.add_char(buffer, char_of_int32(Int32.shift_right(i, 16)));
    Buffer.add_char(buffer, char_of_int32(Int32.shift_right(i, 24)));
};

let write_little_endian_f = (buffer: Buffer.t, f: float) => {
    let i = Int32.of_float(f);
    write_little_endian(buffer, i);
};

let write_header = (data: exr_data) => {
    write_little_endian(data.buffer, magic_number);
    write_little_endian(data.buffer, version);
};

let write_channels_attr = (data: exr_data) => {
    Buffer.add_string(data.buffer, "channels");
    Buffer.add_string(data.buffer, "chlist");

    let size: int =
            2 * 3 +  /* Three channels named B, G, R, plus a null-terminator for each. */
            16 * 3 + /* Four ints (16 bytes) of data per channel. */
            1;       /* One extra null byte. */
    write_little_endian(data.buffer, Int32.of_int(size));

    List.iter((channel) => {
        Buffer.add_string(data.buffer, channel);
        write_little_endian(data.buffer, pixel_type_float);
        write_little_endian(data.buffer, 1l); /* pLinear and reserved */
        write_little_endian(data.buffer, 1l); /* xSampling */
        write_little_endian(data.buffer, 1l); /* ySampling */
    }, ["B", "G", "R"]);

    Buffer.add_char(data.buffer, char_of_int(0)); /* Null terminator */
};

let write_compression_attr = (data: exr_data) => {
    Buffer.add_string(data.buffer, "compression");
    Buffer.add_string(data.buffer, "compression");
    write_little_endian(data.buffer, 1l); /* size = 1 byte */
    Buffer.add_char(data.buffer, compression_none);
};

let write_data_display_window_attrs = (data: exr_data, width: int, height: int)
    =>
{
    let size = 4 * 4; /* 4 ints = 16 bytes */
    let window = [0, 0, width - 1, height - 1];

    Buffer.add_string(data.buffer, "dataWindow");
    Buffer.add_string(data.buffer, "box2i");
    write_little_endian(data.buffer, Int32.of_int(size));
    List.iter((i) => {
        write_little_endian(data.buffer, Int32.of_int(i));
    }, window);

    Buffer.add_string(data.buffer, "displayWindow");
    Buffer.add_string(data.buffer, "box2i");
    write_little_endian(data.buffer, Int32.of_int(size));
    List.iter((i) => {
        write_little_endian(data.buffer, Int32.of_int(i));
    }, window);
};

let write_line_order_attr = (data: exr_data) => {
    Buffer.add_string(data.buffer, "lineOrder");
    Buffer.add_string(data.buffer, "lineOrder");
    write_little_endian(data.buffer, 1l); /* Size = 1 byte. */
    Buffer.add_char(data.buffer, line_order_increasing_y);
};

let write_pixel_aspect_ratio_attr = (data: exr_data) => {
    Buffer.add_string(data.buffer, "pixelAspectRatio");
    Buffer.add_string(data.buffer, "float");
    write_little_endian(data.buffer, 4l); /* 1 float = 4 bytes. */
    write_little_endian_f(data.buffer, 1.0);
};

let write_screen_window_center_attr = (data: exr_data) => {
    Buffer.add_string(data.buffer, "screenWindowCenter");
    Buffer.add_string(data.buffer, "v2f");
    write_little_endian(data.buffer, 8l); /* 2 floats = 8 bytes. */
    write_little_endian_f(data.buffer, 0.0);
    write_little_endian_f(data.buffer, 0.0);
};

let write_screen_window_width = (data: exr_data, width: int) => {
    Buffer.add_string(data.buffer, "screenWindowWidth");
    Buffer.add_string(data.buffer, "float");
    write_little_endian(data.buffer, 4l); /* 1 float = 4 bytes. */
    write_little_endian_f(data.buffer, float_of_int(width));
};
/*
let write_line_offset_table = (data: exr_data, film: Film.film) => {
    let table_size = 8 * film.height; // 1 ulong (8 bytes) per line.
    let data_offset = self.buffer.len() + table_size;

    // Scan line number (int); bytes in line (uint); RGB (3 floats * 4 bytes) per pixel.
    let line_size = 4 + 4 + (film.width * 4 * 3);

    for y in 0..film.height {
        let line_offset = data_offset + y * line_size;
        self.buffer.write_u64::<LittleEndian>(line_offset as u64).unwrap();
    }

    debug_assert!(self.buffer.len() == data_offset);
};
*/
