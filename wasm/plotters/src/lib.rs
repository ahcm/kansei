use base64::engine::general_purpose::STANDARD as BASE64_STD;
use base64::Engine;
use plotters::coord::Shift;
use plotters::prelude::*;
use plotters::series::LineSeries;
use std::slice;

#[unsafe(no_mangle)]
pub extern "C" fn alloc(size: usize) -> *mut u8
{
    let mut buf = Vec::<u8>::with_capacity(size);
    let ptr = buf.as_mut_ptr();
    std::mem::forget(buf);
    ptr
}

#[unsafe(no_mangle)]
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

fn return_string(value: String) -> i64
{
    let bytes = value.into_bytes();
    if bytes.is_empty()
    {
        return 0;
    }
    let len = bytes.len();
    let ptr = alloc(len) as u64;
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr as *mut u8, len);
    }
    ((len as u64) << 32 | (ptr & 0xFFFF_FFFF)) as i64
}

fn slice_from_ptr<T>(ptr: *const T, len: usize) -> Option<&'static [T]>
{
    if ptr.is_null() || len == 0
    {
        return None;
    }
    unsafe { Some(slice::from_raw_parts(ptr, len)) }
}

fn str_from_ptr(ptr: *const u8, len: usize) -> Option<String>
{
    let bytes = slice_from_ptr(ptr, len)?;
    Some(String::from_utf8_lossy(bytes).to_string())
}

fn colors_from_ptr(ptr: *const u32, len: usize) -> Vec<RGBColor>
{
    if let Some(raw) = slice_from_ptr(ptr, len)
    {
        raw.iter()
            .map(|color| {
                let r = ((color >> 16) & 0xFF) as u8;
                let g = ((color >> 8) & 0xFF) as u8;
                let b = (color & 0xFF) as u8;
                RGBColor(r, g, b)
            })
            .collect()
    }
    else
    {
        vec![
            RGBColor(30, 136, 229),
            RGBColor(216, 27, 96),
            RGBColor(0, 150, 136),
            RGBColor(251, 140, 0),
            RGBColor(94, 53, 177),
        ]
    }
}

fn unpack_series(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
) -> Option<Vec<Vec<(f64, f64)>>>
{
    let data = slice_from_ptr(data_ptr, data_len)?;
    let series_counts = slice_from_ptr(series_ptr, series_len)?;
    let mut expected = 0usize;
    for count in series_counts
    {
        expected = expected.saturating_add(*count as usize * 2);
    }
    if expected != data_len
    {
        return None;
    }

    let mut result = Vec::with_capacity(series_counts.len());
    let mut offset = 0usize;
    for count in series_counts
    {
        let count = *count as usize;
        let mut points = Vec::with_capacity(count);
        for _ in 0..count
        {
            if offset + 1 >= data_len
            {
                return None;
            }
            let x = data[offset];
            let y = data[offset + 1];
            points.push((x, y));
            offset += 2;
        }
        result.push(points);
    }
    Some(result)
}

fn compute_range(series: &[Vec<(f64, f64)>]) -> Option<(f64, f64, f64, f64)>
{
    let mut min_x = None;
    let mut max_x = None;
    let mut min_y = None;
    let mut max_y = None;
    for points in series
    {
        for (x, y) in points
        {
            min_x = Some(min_x.map_or(*x, |v: f64| v.min(*x)));
            max_x = Some(max_x.map_or(*x, |v: f64| v.max(*x)));
            min_y = Some(min_y.map_or(*y, |v: f64| v.min(*y)));
            max_y = Some(max_y.map_or(*y, |v: f64| v.max(*y)));
        }
    }
    match (min_x, max_x, min_y, max_y)
    {
        (Some(min_x), Some(max_x), Some(min_y), Some(max_y)) => Some((min_x, max_x, min_y, max_y)),
        _ => None,
    }
}

fn parse_tsv_points(tsv: &str, x_col: usize, y_col: usize) -> Vec<(f64, f64)>
{
    let mut points = Vec::new();
    for line in tsv.lines()
    {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#')
        {
            continue;
        }
        let mut x_val = None;
        let mut y_val = None;
        for (idx, part) in trimmed.split('\t').enumerate()
        {
            if idx == x_col
            {
                if let Ok(v) = part.trim().parse::<f64>()
                {
                    x_val = Some(v);
                }
            }
            if idx == y_col
            {
                if let Ok(v) = part.trim().parse::<f64>()
                {
                    y_val = Some(v);
                }
            }
            if x_val.is_some() && y_val.is_some()
            {
                break;
            }
        }
        if let (Some(x), Some(y)) = (x_val, y_val)
        {
            points.push((x, y));
        }
    }
    points
}

fn normalize_range(min_x: f64, max_x: f64, min_y: f64, max_y: f64) -> (f64, f64, f64, f64)
{
    let mut min_x = min_x;
    let mut max_x = max_x;
    let mut min_y = min_y;
    let mut max_y = max_y;
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
    (min_x, max_x, min_y, max_y)
}

fn draw_line_chart<B: DrawingBackend>(
    root: DrawingArea<B, Shift>,
    series: &[Vec<(f64, f64)>],
    colors: &[RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<B::ErrorType>>
{
    root.fill(&WHITE)?;
    let mut chart = ChartBuilder::on(&root)
        .margin(10)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 30)
        .caption(title.unwrap_or_default(), ("sans-serif", 18))
        .build_cartesian_2d(min_x..max_x, min_y..max_y)?;
    let mut mesh = chart.configure_mesh();
    if let Some(label) = x_label
    {
        mesh.x_desc(label);
    }
    if let Some(label) = y_label
    {
        mesh.y_desc(label);
    }
    mesh.draw()?;

    for (idx, points) in series.iter().enumerate()
    {
        let color = colors.get(idx).cloned().unwrap_or(RGBColor(30, 136, 229));
        chart.draw_series(LineSeries::new(points.clone(), &color))?;
    }
    root.present()?;
    Ok(())
}

fn draw_scatter_chart<B: DrawingBackend>(
    root: DrawingArea<B, Shift>,
    series: &[Vec<(f64, f64)>],
    colors: &[RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<B::ErrorType>>
{
    root.fill(&WHITE)?;
    let mut chart = ChartBuilder::on(&root)
        .margin(10)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 30)
        .caption(title.unwrap_or_default(), ("sans-serif", 18))
        .build_cartesian_2d(min_x..max_x, min_y..max_y)?;
    let mut mesh = chart.configure_mesh();
    if let Some(label) = x_label
    {
        mesh.x_desc(label);
    }
    if let Some(label) = y_label
    {
        mesh.y_desc(label);
    }
    mesh.draw()?;

    for (idx, points) in series.iter().enumerate()
    {
        let color = colors.get(idx).cloned().unwrap_or(RGBColor(30, 136, 229));
        chart.draw_series(points.iter().map(|(x, y)| {
            Circle::new((*x, *y), 3, ShapeStyle::from(&color).filled())
        }))?;
    }
    root.present()?;
    Ok(())
}

fn draw_bar_chart<B: DrawingBackend>(
    root: DrawingArea<B, Shift>,
    series: &[Vec<(f64, f64)>],
    colors: &[RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<B::ErrorType>>
{
    if series.is_empty()
    {
        return Ok(());
    }
    let first = &series[0];
    for points in series
    {
        if points.len() != first.len()
        {
            return Ok(());
        }
    }

    root.fill(&WHITE)?;
    let mut chart = ChartBuilder::on(&root)
        .margin(10)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 30)
        .caption(title.unwrap_or_default(), ("sans-serif", 18))
        .build_cartesian_2d(min_x..max_x, min_y..max_y)?;
    let mut mesh = chart.configure_mesh();
    if let Some(label) = x_label
    {
        mesh.x_desc(label);
    }
    if let Some(label) = y_label
    {
        mesh.y_desc(label);
    }
    mesh.draw()?;

    let group_width = 0.8;
    let bar_width = group_width / series.len() as f64;
    for (series_idx, points) in series.iter().enumerate()
    {
        let color = colors.get(series_idx).cloned().unwrap_or(RGBColor(30, 136, 229));
        chart.draw_series(points.iter().map(|(x, y)| {
            let offset = (series_idx as f64 * bar_width) - (group_width / 2.0);
            let x0 = x + offset;
            let x1 = x0 + bar_width;
            let y0 = 0.0f64.min(*y);
            let y1 = 0.0f64.max(*y);
            Rectangle::new([(x0, y0), (x1, y1)], ShapeStyle::from(&color).filled())
        }))?;
    }
    root.present()?;
    Ok(())
}

type SvgDrawFn = for<'a> fn(
    DrawingArea<SVGBackend<'a>, Shift>,
    &'a [Vec<(f64, f64)>],
    &'a [RGBColor],
    f64,
    f64,
    f64,
    f64,
    Option<String>,
    Option<String>,
    Option<String>,
) -> Result<(), DrawingAreaErrorKind<<SVGBackend<'a> as DrawingBackend>::ErrorType>>;

type PngDrawFn = for<'a> fn(
    DrawingArea<BitMapBackend<'a>, Shift>,
    &'a [Vec<(f64, f64)>],
    &'a [RGBColor],
    f64,
    f64,
    f64,
    f64,
    Option<String>,
    Option<String>,
    Option<String>,
) -> Result<(), DrawingAreaErrorKind<<BitMapBackend<'a> as DrawingBackend>::ErrorType>>;

fn build_chart_svg(
    series: Vec<Vec<(f64, f64)>>,
    colors: Vec<RGBColor>,
    width: u32,
    height: u32,
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
    draw: SvgDrawFn,
) -> i64
{
    let mut svg = String::new();
    let backend = SVGBackend::with_string(&mut svg, (width, height));
    let root = backend.into_drawing_area();
    let result = (draw)(
        root,
        &series,
        &colors,
        min_x,
        max_x,
        min_y,
        max_y,
        title,
        x_label,
        y_label,
    );
    if result.is_err()
    {
        return 0;
    }
    return_string(svg)
}

fn draw_line_chart_svg<'a>(
    root: DrawingArea<SVGBackend<'a>, Shift>,
    series: &'a [Vec<(f64, f64)>],
    colors: &'a [RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<<SVGBackend<'a> as DrawingBackend>::ErrorType>>
{
    draw_line_chart(
        root, series, colors, min_x, max_x, min_y, max_y, title, x_label, y_label,
    )
}

fn draw_line_chart_png<'a>(
    root: DrawingArea<BitMapBackend<'a>, Shift>,
    series: &'a [Vec<(f64, f64)>],
    colors: &'a [RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<<BitMapBackend<'a> as DrawingBackend>::ErrorType>>
{
    draw_line_chart(
        root, series, colors, min_x, max_x, min_y, max_y, title, x_label, y_label,
    )
}

fn draw_scatter_chart_svg<'a>(
    root: DrawingArea<SVGBackend<'a>, Shift>,
    series: &'a [Vec<(f64, f64)>],
    colors: &'a [RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<<SVGBackend<'a> as DrawingBackend>::ErrorType>>
{
    draw_scatter_chart(
        root, series, colors, min_x, max_x, min_y, max_y, title, x_label, y_label,
    )
}

fn draw_scatter_chart_png<'a>(
    root: DrawingArea<BitMapBackend<'a>, Shift>,
    series: &'a [Vec<(f64, f64)>],
    colors: &'a [RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<<BitMapBackend<'a> as DrawingBackend>::ErrorType>>
{
    draw_scatter_chart(
        root, series, colors, min_x, max_x, min_y, max_y, title, x_label, y_label,
    )
}

fn draw_bar_chart_svg<'a>(
    root: DrawingArea<SVGBackend<'a>, Shift>,
    series: &'a [Vec<(f64, f64)>],
    colors: &'a [RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<<SVGBackend<'a> as DrawingBackend>::ErrorType>>
{
    draw_bar_chart(
        root, series, colors, min_x, max_x, min_y, max_y, title, x_label, y_label,
    )
}

fn draw_bar_chart_png<'a>(
    root: DrawingArea<BitMapBackend<'a>, Shift>,
    series: &'a [Vec<(f64, f64)>],
    colors: &'a [RGBColor],
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
) -> Result<(), DrawingAreaErrorKind<<BitMapBackend<'a> as DrawingBackend>::ErrorType>>
{
    draw_bar_chart(
        root, series, colors, min_x, max_x, min_y, max_y, title, x_label, y_label,
    )
}

fn build_chart_png(
    series: Vec<Vec<(f64, f64)>>,
    colors: Vec<RGBColor>,
    width: u32,
    height: u32,
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    title: Option<String>,
    x_label: Option<String>,
    y_label: Option<String>,
    draw: PngDrawFn,
) -> i64
{
    let mut buffer = vec![0u8; (width as usize) * (height as usize) * 3];
    let backend = BitMapBackend::with_buffer(&mut buffer, (width, height));
    let root = backend.into_drawing_area();
    let result = (draw)(
        root,
        &series,
        &colors,
        min_x,
        max_x,
        min_y,
        max_y,
        title,
        x_label,
        y_label,
    );
    if result.is_err()
    {
        return 0;
    }
    let mut png_bytes = Vec::new();
    {
        let encoder = image::codecs::png::PngEncoder::new(&mut png_bytes);
        if encoder
            .encode(
                &buffer,
                width,
                height,
                image::ColorType::Rgb8,
            )
            .is_err()
        {
            return 0;
        }
    }
    let encoded = BASE64_STD.encode(png_bytes);
    return_string(encoded)
}

#[unsafe(no_mangle)]
pub extern "C" fn line_chart_svg_str(
    data_ptr: *const f64,
    data_len: usize,
    width: u32,
    height: u32,
) -> i64
{
    if data_ptr.is_null() || data_len < 2 || data_len % 2 != 0
    {
        return 0;
    }
    let data = unsafe { slice::from_raw_parts(data_ptr, data_len) };
    let mut points = Vec::with_capacity(data_len / 2);
    for idx in (0..data_len).step_by(2)
    {
        points.push((data[idx], data[idx + 1]));
    }
    let series = vec![points];
    let (min_x, max_x, min_y, max_y) = match compute_range(&series)
    {
        Some(range) => range,
        None => return 0,
    };
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_svg(
        series,
        colors_from_ptr(std::ptr::null(), 0),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        None,
        None,
        None,
        draw_line_chart_svg,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn line_chart_svg_str_multi(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
    colors_ptr: *const u32,
    colors_len: usize,
    width: u32,
    height: u32,
    mut min_x: f64,
    mut max_x: f64,
    mut min_y: f64,
    mut max_y: f64,
    title_ptr: *const u8,
    title_len: usize,
    x_ptr: *const u8,
    x_len: usize,
    y_ptr: *const u8,
    y_len: usize,
) -> i64
{
    let series = match unpack_series(data_ptr, data_len, series_ptr, series_len)
    {
        Some(series) => series,
        None => return 0,
    };
    if min_x >= max_x || min_y >= max_y
    {
        if let Some((rx0, rx1, ry0, ry1)) = compute_range(&series)
        {
            min_x = rx0;
            max_x = rx1;
            min_y = ry0;
            max_y = ry1;
        }
    }
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_svg(
        series,
        colors_from_ptr(colors_ptr, colors_len),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        str_from_ptr(title_ptr, title_len),
        str_from_ptr(x_ptr, x_len),
        str_from_ptr(y_ptr, y_len),
        draw_line_chart_svg,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn line_chart_png_str_multi(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
    colors_ptr: *const u32,
    colors_len: usize,
    width: u32,
    height: u32,
    mut min_x: f64,
    mut max_x: f64,
    mut min_y: f64,
    mut max_y: f64,
    title_ptr: *const u8,
    title_len: usize,
    x_ptr: *const u8,
    x_len: usize,
    y_ptr: *const u8,
    y_len: usize,
) -> i64
{
    let series = match unpack_series(data_ptr, data_len, series_ptr, series_len)
    {
        Some(series) => series,
        None => return 0,
    };
    if min_x >= max_x || min_y >= max_y
    {
        if let Some((rx0, rx1, ry0, ry1)) = compute_range(&series)
        {
            min_x = rx0;
            max_x = rx1;
            min_y = ry0;
            max_y = ry1;
        }
    }
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_png(
        series,
        colors_from_ptr(colors_ptr, colors_len),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        str_from_ptr(title_ptr, title_len),
        str_from_ptr(x_ptr, x_len),
        str_from_ptr(y_ptr, y_len),
        draw_line_chart_png,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn scatter_chart_svg_str_multi(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
    colors_ptr: *const u32,
    colors_len: usize,
    width: u32,
    height: u32,
    mut min_x: f64,
    mut max_x: f64,
    mut min_y: f64,
    mut max_y: f64,
    title_ptr: *const u8,
    title_len: usize,
    x_ptr: *const u8,
    x_len: usize,
    y_ptr: *const u8,
    y_len: usize,
) -> i64
{
    let series = match unpack_series(data_ptr, data_len, series_ptr, series_len)
    {
        Some(series) => series,
        None => return 0,
    };
    if min_x >= max_x || min_y >= max_y
    {
        if let Some((rx0, rx1, ry0, ry1)) = compute_range(&series)
        {
            min_x = rx0;
            max_x = rx1;
            min_y = ry0;
            max_y = ry1;
        }
    }
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_svg(
        series,
        colors_from_ptr(colors_ptr, colors_len),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        str_from_ptr(title_ptr, title_len),
        str_from_ptr(x_ptr, x_len),
        str_from_ptr(y_ptr, y_len),
        draw_scatter_chart_svg,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn scatter_chart_png_str_multi(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
    colors_ptr: *const u32,
    colors_len: usize,
    width: u32,
    height: u32,
    mut min_x: f64,
    mut max_x: f64,
    mut min_y: f64,
    mut max_y: f64,
    title_ptr: *const u8,
    title_len: usize,
    x_ptr: *const u8,
    x_len: usize,
    y_ptr: *const u8,
    y_len: usize,
) -> i64
{
    let series = match unpack_series(data_ptr, data_len, series_ptr, series_len)
    {
        Some(series) => series,
        None => return 0,
    };
    if min_x >= max_x || min_y >= max_y
    {
        if let Some((rx0, rx1, ry0, ry1)) = compute_range(&series)
        {
            min_x = rx0;
            max_x = rx1;
            min_y = ry0;
            max_y = ry1;
        }
    }
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_png(
        series,
        colors_from_ptr(colors_ptr, colors_len),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        str_from_ptr(title_ptr, title_len),
        str_from_ptr(x_ptr, x_len),
        str_from_ptr(y_ptr, y_len),
        draw_scatter_chart_png,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn scatter_tsv_svg_str(
    tsv_ptr: *const u8,
    tsv_len: usize,
    x_col: i32,
    y_col: i32,
    width: u32,
    height: u32,
) -> i64
{
    if x_col < 0 || y_col < 0
    {
        return 0;
    }
    let tsv = match str_from_ptr(tsv_ptr, tsv_len)
    {
        Some(v) => v,
        None => return 0,
    };
    let points = parse_tsv_points(&tsv, x_col as usize, y_col as usize);
    if points.is_empty()
    {
        return 0;
    }
    let series = vec![points];
    let (min_x, max_x, min_y, max_y) = match compute_range(&series)
    {
        Some(range) => range,
        None => return 0,
    };
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_svg(
        series,
        colors_from_ptr(std::ptr::null(), 0),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        None,
        None,
        None,
        draw_scatter_chart_svg,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn scatter_tsv_png_str(
    tsv_ptr: *const u8,
    tsv_len: usize,
    x_col: i32,
    y_col: i32,
    width: u32,
    height: u32,
) -> i64
{
    if x_col < 0 || y_col < 0
    {
        return 0;
    }
    let tsv = match str_from_ptr(tsv_ptr, tsv_len)
    {
        Some(v) => v,
        None => return 0,
    };
    let points = parse_tsv_points(&tsv, x_col as usize, y_col as usize);
    if points.is_empty()
    {
        return 0;
    }
    let series = vec![points];
    let (min_x, max_x, min_y, max_y) = match compute_range(&series)
    {
        Some(range) => range,
        None => return 0,
    };
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_png(
        series,
        colors_from_ptr(std::ptr::null(), 0),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        None,
        None,
        None,
        draw_scatter_chart_png,
    )
}
#[unsafe(no_mangle)]
pub extern "C" fn bar_chart_svg_str_multi(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
    colors_ptr: *const u32,
    colors_len: usize,
    width: u32,
    height: u32,
    mut min_x: f64,
    mut max_x: f64,
    mut min_y: f64,
    mut max_y: f64,
    title_ptr: *const u8,
    title_len: usize,
    x_ptr: *const u8,
    x_len: usize,
    y_ptr: *const u8,
    y_len: usize,
) -> i64
{
    let series = match unpack_series(data_ptr, data_len, series_ptr, series_len)
    {
        Some(series) => series,
        None => return 0,
    };
    if min_x >= max_x || min_y >= max_y
    {
        if let Some((rx0, rx1, ry0, ry1)) = compute_range(&series)
        {
            min_x = rx0;
            max_x = rx1;
            min_y = ry0;
            max_y = ry1;
        }
    }
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_svg(
        series,
        colors_from_ptr(colors_ptr, colors_len),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        str_from_ptr(title_ptr, title_len),
        str_from_ptr(x_ptr, x_len),
        str_from_ptr(y_ptr, y_len),
        draw_bar_chart_svg,
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn bar_chart_png_str_multi(
    data_ptr: *const f64,
    data_len: usize,
    series_ptr: *const u32,
    series_len: usize,
    colors_ptr: *const u32,
    colors_len: usize,
    width: u32,
    height: u32,
    mut min_x: f64,
    mut max_x: f64,
    mut min_y: f64,
    mut max_y: f64,
    title_ptr: *const u8,
    title_len: usize,
    x_ptr: *const u8,
    x_len: usize,
    y_ptr: *const u8,
    y_len: usize,
) -> i64
{
    let series = match unpack_series(data_ptr, data_len, series_ptr, series_len)
    {
        Some(series) => series,
        None => return 0,
    };
    if min_x >= max_x || min_y >= max_y
    {
        if let Some((rx0, rx1, ry0, ry1)) = compute_range(&series)
        {
            min_x = rx0;
            max_x = rx1;
            min_y = ry0;
            max_y = ry1;
        }
    }
    let (min_x, max_x, min_y, max_y) = normalize_range(min_x, max_x, min_y, max_y);
    build_chart_png(
        series,
        colors_from_ptr(colors_ptr, colors_len),
        width,
        height,
        min_x,
        max_x,
        min_y,
        max_y,
        str_from_ptr(title_ptr, title_len),
        str_from_ptr(x_ptr, x_len),
        str_from_ptr(y_ptr, y_len),
        draw_bar_chart_png,
    )
}
