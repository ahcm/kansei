use plotters::prelude::*;
use std::slice;

#[no_mangle]
pub extern "C" fn alloc(size: usize) -> *mut u8
{
    let mut buf = Vec::<u8>::with_capacity(size);
    let ptr = buf.as_mut_ptr();
    std::mem::forget(buf);
    ptr
}

#[no_mangle]
pub extern "C" fn dealloc(ptr: *mut u8, size: usize)
{
    if ptr.is_null() || size == 0
    {
        return;
    }
    unsafe {
        let _ = Vec::from_raw_parts(ptr, 0, size);
    }
}

#[no_mangle]
pub extern "C" fn line_chart_svg_str(
    data_ptr: *const f64,
    data_len: usize,
    width: u32,
    height: u32,
) -> i64
{
    if data_ptr.is_null() || data_len < 2
    {
        return 0;
    }

    let count = data_len / 2;
    let data = unsafe { slice::from_raw_parts(data_ptr, count * 2) };
    let mut points = Vec::with_capacity(count);
    for idx in 0..count
    {
        let x = data[idx * 2];
        let y = data[idx * 2 + 1];
        points.push((x, y));
    }

    let mut min_x = points[0].0;
    let mut max_x = points[0].0;
    let mut min_y = points[0].1;
    let mut max_y = points[0].1;
    for (x, y) in &points
    {
        if *x < min_x { min_x = *x; }
        if *x > max_x { max_x = *x; }
        if *y < min_y { min_y = *y; }
        if *y > max_y { max_y = *y; }
    }

    if min_x == max_x
    {
        min_x -= 1.0;
        max_x += 1.0;
    }
    if min_y == max_y
    {
        min_y -= 1.0;
        max_y += 1.0;
    }

    let mut svg = String::new();
    let backend = SVGBackend::with_string(&mut svg, (width, height));
    let root = backend.into_drawing_area();
    if root.fill(&WHITE).is_err()
    {
        return 0;
    }

    let mut chart = match ChartBuilder::on(&root)
        .margin(10)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 30)
        .build_cartesian_2d(min_x..max_x, min_y..max_y)
    {
        Ok(chart) => chart,
        Err(_) => return 0,
    };

    if chart.configure_mesh().draw().is_err()
    {
        return 0;
    }

    if chart
        .draw_series(LineSeries::new(points, &BLUE))
        .is_err()
    {
        return 0;
    }

    if root.present().is_err()
    {
        return 0;
    }

    let bytes = svg.into_bytes();
    let len = bytes.len();
    if len == 0
    {
        return 0;
    }
    let ptr = alloc(len) as u64;
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr as *mut u8, len);
    }
    ((len as u64) << 32 | (ptr & 0xFFFF_FFFF)) as i64
}
