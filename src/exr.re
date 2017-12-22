type t = {
    static_buffer: Buffer.t,
    data_buffer: Buffer.t,
    width: int,
    height: int,
};

module Constants = {
    let magic_number: int32 = 20000630l;
    let version: int32 = 2l;
    let pixel_type_float: int32 = 2l;
    let compression_none: char = char_of_int(0);
    let line_order_increasing_y: char = char_of_int(0);
};

let _char_of_int32 = (i: int32) => {
    char_of_int(Int32.to_int(i) land 0xff)
};

let _char_of_int64 = (i: int64) => {
    char_of_int(Int64.to_int(i) land 0xff)
};

let _write_little_endian_32 = (buffer: Buffer.t, i: int32) => {
    Buffer.add_char(buffer, _char_of_int32(i));
    Buffer.add_char(buffer, _char_of_int32(Int32.shift_right(i, 8)));
    Buffer.add_char(buffer, _char_of_int32(Int32.shift_right(i, 16)));
    Buffer.add_char(buffer, _char_of_int32(Int32.shift_right(i, 24)));
};

let _write_little_endian_64 = (buffer: Buffer.t, i: int64) => {
    Buffer.add_char(buffer, _char_of_int64(i));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 8)));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 16)));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 24)));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 32)));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 40)));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 48)));
    Buffer.add_char(buffer, _char_of_int64(Int64.shift_right(i, 56)));
};

let _write_little_endian_f = (buffer: Buffer.t, f: float) => {
    let i = Int32.bits_of_float(f);
    _write_little_endian_32(buffer, i);
};

let _write_header = (buffer: Buffer.t) => {
    _write_little_endian_32(buffer, Constants.magic_number);
    _write_little_endian_32(buffer, Constants.version);
};

let _write_channels_attr = (buffer: Buffer.t) => {
    Buffer.add_string(buffer, "channels\x00");
    Buffer.add_string(buffer, "chlist\x00");

    let size: int =
            2 * 3 +  /* Three channels named B, G, R, plus a null-terminator for each. */
            16 * 3 + /* Four ints (16 bytes) of data per channel. */
            1;       /* One extra null byte. */
    _write_little_endian_32(buffer, Int32.of_int(size));

    List.iter((channel) => {
        Buffer.add_string(buffer, channel);
        _write_little_endian_32(buffer, Constants.pixel_type_float);
        _write_little_endian_32(buffer, 1l); /* pLinear and reserved */
        _write_little_endian_32(buffer, 1l); /* xSampling */
        _write_little_endian_32(buffer, 1l); /* ySampling */
    }, ["B\x00", "G\x00", "R\x00"]);

    Buffer.add_char(buffer, char_of_int(0)); /* Null terminator */
};

let _write_compression_attr = (buffer: Buffer.t) => {
    Buffer.add_string(buffer, "compression\x00");
    Buffer.add_string(buffer, "compression\x00");
    _write_little_endian_32(buffer, 1l); /* size = 1 byte */
    Buffer.add_char(buffer, Constants.compression_none);
};

let _write_data_display_window_attrs = (buffer: Buffer.t, width: int, height: int)
    =>
{
    let size = 4 * 4; /* 4 ints = 16 bytes */
    let window = [0, 0, width - 1, height - 1];

    Buffer.add_string(buffer, "dataWindow\x00");
    Buffer.add_string(buffer, "box2i\x00");
    _write_little_endian_32(buffer, Int32.of_int(size));
    List.iter((i) => {
        _write_little_endian_32(buffer, Int32.of_int(i));
    }, window);

    Buffer.add_string(buffer, "displayWindow\x00");
    Buffer.add_string(buffer, "box2i\x00");
    _write_little_endian_32(buffer, Int32.of_int(size));
    List.iter((i) => {
        _write_little_endian_32(buffer, Int32.of_int(i));
    }, window);
};

let _write_line_order_attr = (buffer: Buffer.t) => {
    Buffer.add_string(buffer, "lineOrder\x00");
    Buffer.add_string(buffer, "lineOrder\x00");
    _write_little_endian_32(buffer, 1l); /* Size = 1 byte. */
    Buffer.add_char(buffer, Constants.line_order_increasing_y);
};

let _write_pixel_aspect_ratio_attr = (buffer: Buffer.t) => {
    Buffer.add_string(buffer, "pixelAspectRatio\x00");
    Buffer.add_string(buffer, "float\x00");
    _write_little_endian_32(buffer, 4l); /* 1 float = 4 bytes. */
    _write_little_endian_f(buffer, 1.0);
};

let _write_screen_window_center_attr = (buffer: Buffer.t) => {
    Buffer.add_string(buffer, "screenWindowCenter\x00");
    Buffer.add_string(buffer, "v2f\x00");
    _write_little_endian_32(buffer, 8l); /* 2 floats = 8 bytes. */
    _write_little_endian_f(buffer, 0.0);
    _write_little_endian_f(buffer, 0.0);
};

let _write_screen_window_width = (buffer: Buffer.t, width: int) => {
    Buffer.add_string(buffer, "screenWindowWidth\x00");
    Buffer.add_string(buffer, "float\x00");
    _write_little_endian_32(buffer, 4l); /* 1 float = 4 bytes. */
    _write_little_endian_f(buffer, float_of_int(width));
};

let _write_line_offset_table = (buffer: Buffer.t, width: int, height: int) => {
    let table_size = 8 * height; /* 1 ulong (8 bytes) per line */
    let data_offset = Buffer.length(buffer) + table_size;
    print_endline(Printf.sprintf("data_offset=%d", data_offset));

    /* Scan line number (int); bytes in line (uint); RGB (3 floats * 4 bytes) per pixel. */
    let line_size = 4 + 4 + (width * 4 * 3);

    for (y in 0 to (height - 1)) {
        let line_offset = data_offset + y * line_size;
        _write_little_endian_64(buffer, Int64.of_int(line_offset));
    };

    assert(Buffer.length(buffer) == data_offset);
};

/* Precondition: enough space was allocated in buffer (e.g. via Exr.create function). */
let _write_channels = (buffer: Buffer.t, film: Film.t) => {
    /* Remove previous channel data. */
    Buffer.reset(buffer);

    /* Scan line number (int); bytes in line (uint); RGB (3 floats * 4 bytes) per pixel. */
    let line_size = 4 + 4 + (film.width * 4 * 3);

    for (y in 0 to (film.height - 1)) {
        _write_little_endian_32(buffer, Int32.of_int(y)); /* Scan line number */
        _write_little_endian_32(buffer, Int32.of_int(line_size - 8)); /* Bytes in line */

        for (x in 0 to (film.width - 1)) {
            let pixel = film.pixels[film.height - y - 1][x];
            _write_little_endian_f(buffer, pixel.accum.z /. pixel.weight);
        };
        for (x in 0 to (film.width - 1)) {
            let pixel = film.pixels[film.height - y - 1][x];
            _write_little_endian_f(buffer, pixel.accum.y /. pixel.weight);
        };
        for (x in 0 to (film.width - 1)) {
            let pixel = film.pixels[film.height - y - 1][x];
            _write_little_endian_f(buffer, pixel.accum.x /. pixel.weight);
        };
    };
};

let create = (width: int, height: int) => {
    let static_buffer = Buffer.create(4096);

    /* Begin header */
    _write_header(static_buffer);
    _write_channels_attr(static_buffer);
    _write_compression_attr(static_buffer);
    _write_data_display_window_attrs(static_buffer, width, height);
    _write_line_order_attr(static_buffer);
    _write_pixel_aspect_ratio_attr(static_buffer);
    _write_screen_window_center_attr(static_buffer);
    _write_screen_window_width(static_buffer, width);
    Buffer.add_char(static_buffer, char_of_int(0)); /* End header */

    /* Begin line offset table */
    _write_line_offset_table(static_buffer, width, height); /* End line offset table */

    /* Scan line number (int); bytes in line (uint); RGB (3 floats * 4 bytes) per pixel. */
    let line_size = 4 + 4 + (width * 4 * 3);
    let data_size = height * line_size;

    {
        static_buffer: static_buffer,
        data_buffer: Buffer.create(data_size),
        width: width,
        height: height,
    }
};

let update = (data: t, film: Film.t) => {
    if (data.width != film.width || data.height != film.height) {
        false
    }
    else {
        _write_channels(data.data_buffer, film);
        true
    }
};

let output_exr = (chan: out_channel, data: t) => {
    if (Buffer.length(data.data_buffer) == 0) {
        false
    }
    else {
        Buffer.output_buffer(chan, data.static_buffer);
        Buffer.output_buffer(chan, data.data_buffer);
        true
    }
};
