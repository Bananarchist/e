use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum Spot {
    Shaded,
    Lit,
    Opaque,
}

fn spot_to_string(spot: &Spot) -> &str {
    match spot {
        Spot::Shaded => ".",
        Spot::Opaque => "#",
        Spot::Lit => "*",
    }
}

fn spot_grid_2d_to_string<'a>(grid: &'a Vec<Spot>, width: usize, height: usize) -> String {
    grid.iter()
        .fold((0, 0, String::new()), |(x, y, mut ret), s| {
            if x == (width - 1) {
                ret += spot_to_string(s);
                ret.push('\n');
                (0, y + 1, ret)
            } else {
                ret += spot_to_string(s);
                (x + 1, y, ret)
            }
        })
        .2
}

fn pairs<T: Copy>(v: &Vec<T>) -> Option<Vec<(T, T)>> {
    if v.len() > 1 {
        let mut pairs: Vec<(T, T)> = vec![];
        let mut i = 0;
        loop {
            if i < v.len() - 1 {
                pairs.push((v[i], v[i + 1]));
            } else {
                pairs.push((v[i], v[0]));
                break;
            }
            i += 1;
        }
        Some(pairs)
    } else {
        None
    }
}

fn occlusion_map(
    lights: Vec<(usize, usize)>,
    objects: Vec<Vec<(usize, usize)>>,
    width: usize,
    height: usize,
) -> Vec<Spot> {
    let mut hmap: HashMap<(usize, usize), Spot> = HashMap::new();
    for y in 0..height {
        for x in 0..width {
            hmap.insert((x, y), Spot::Shaded);
        }
    }
    let mut coords: Vec<Spot> = vec![];
    coords.resize(width * height, Spot::Shaded);

    for object in objects {
        if object.len() > 1 {
            match pairs(&object) {
                Some(ps) => {
                    println!("{:?}", ps);
                    for pair in ps {
                        let δx = (pair.1 .0 as isize) - (pair.0 .0 as isize);
                        let δy = (pair.1 .1 as isize) - (pair.0 .1 as isize);
                        let x_change: f64 = if δx != 0 {
                            (δx as f64) / (δy as f64)
                        } else {
                            0.0
                        };
                        let y_change: f64 = if δy != 0 {
                            (if x_change < 0.0 { -1.0 } else { 1.0 }) * (δy as f64) / (δx as f64)
                        } else {
                            0.0
                        };
                        let mut e_x: f64 = 0.0;
                        let mut e_y: f64 = 0.0;
                        let mut x: usize = pair.0 .0;
                        let mut y: usize = pair.0 .1;
                        loop {
                            match hmap.get(&(x, y)) {
                                Some(Spot::Shaded) => {
                                    hmap.insert((x, y), Spot::Opaque);
                                    coords[y * width + x] = Spot::Opaque;
                                }
                                _ => (),
                            }
                            e_x += x_change;
                            e_y += y_change;
                            if e_x <= -1.0 {
                                if x == 0 || x == pair.1 .0 {
                                    break;
                                }
                                x -= 1;
                                e_x += 1.0;
                            }
                            if e_x >= 1.0 {
                                x += 1;
                                e_x -= 1.0;
                                if x == width || x == pair.1 .0 {
                                    break;
                                }
                            }
                            if e_y <= -1.0 {
                                if y == 0 || y == pair.1 .1 {
                                    break;
                                }
                                y -= 1;
                                e_y += 1.0;
                            }
                            if e_y >= 1.0 {
                                y += 1;
                                e_y -= 1.0;
                                if y == height || y == pair.1 .1 {
                                    break;
                                }
                            }
                        }
                    }
                    // fill between lines
                    // since we can know the minimal and maximal x's and y's, we can make this
                    // better someday
                    for y in 0..height {
                        let mut opaque = false;
                        for x in 0..width {
                            match hmap.get(&(x,y)) {
                                Some(Spot::Opaque) => {
                                    if hmap.get(&(x - 1, y)) == Some(&Spot::Opaque) || hmap.get(&(x+1, y)) == Some(&Spot::Opaque) {
                                        opaque = false;
                                    } else if object.contains(&(x,y)) {
                                        opaque = false;
                                    } else {
                                        opaque = !opaque;
                                    }
                                }
                                _ => {
                                    if opaque {
                                        coords[y * width + x] = Spot::Opaque;
                                        hmap.insert((x,y), Spot::Opaque);
                                    }
                                },
                            }
                        }
                    }

                },
                None => (),
            }
        } else {
            for pt in object {
                hmap.insert(pt, Spot::Opaque);
                coords[pt.1 * width + pt.0] = Spot::Opaque;
            }
        }
    }

    for light in lights {
        for degree in 0..360 {
            let angle: f64 = f64::from(degree); // we should precalculate this
            let slope: f64 = angle.tan();
            let x_change: f64 = angle.cos();
            let y_change: f64 = angle.sin();
            let mut e_y: f64 = 0.0;
            let mut e_x: f64 = 0.0;
            let mut e: f64 = 0.0;
            let mut x: usize = light.0;
            let mut y: usize = light.1;
            let slope_negative = slope < 0.0;
            let xneg = degree >= 180;
            loop {
                match hmap.get(&(x, y)) {
                    Some(Spot::Shaded) => {
                        hmap.insert((x, y), Spot::Lit);
                        coords[y * width + x] = Spot::Lit;
                    }
                    Some(Spot::Opaque) => break,
                    _ => (),
                }
                //e += slope;
                e_x += x_change;
                e_y += y_change;
                if e_x <= -1.0 {
                    if x == 0 {
                        break;
                    }
                    x -= 1;
                    e_x += 1.0;
                } else if e_x >= 1.0 {
                    x += 1;
                    e_x -= 1.0;
                    if x == width {
                        break;
                    }
                }
                if e_y <= -1.0 {
                    if y == 0 {
                        break;
                    }
                    y -= 1;
                    e_y += 1.0;
                } else if e_y >= 1.0 {
                    y += 1;
                    e_y -= 1.0;
                    if y == height {
                        break;
                    }
                }
            }
        }
    }
    coords
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn light_illuminates_empty_space() {
        let mut expected = vec![];
        expected.resize(100, Spot::Lit);
        let result = occlusion_map(vec![(2, 2)], vec![], 5, 20);
        assert_eq!(result, expected);
    }
    #[test]
    fn objects_cast_shadows() -> Result<(), String> {
        let mut expected = vec![];
        expected.resize(100, Spot::Lit);
        expected[4 * 10 + 4] = Spot::Opaque;
        expected[4 * 10 + 5] = Spot::Opaque;
        expected[4 * 10 + 6] = Spot::Opaque;
        expected[4 * 10 + 7] = Spot::Opaque;
        expected[4 * 10 + 8] = Spot::Opaque;
        expected[5 * 10 + 4] = Spot::Opaque;
        expected[6 * 10 + 4] = Spot::Opaque;
        expected[7 * 10 + 4] = Spot::Opaque;
        expected[8 * 10 + 4] = Spot::Opaque;
        expected[5 * 10 + 7] = Spot::Opaque;
        expected[6 * 10 + 6] = Spot::Opaque;
        expected[7 * 10 + 5] = Spot::Opaque;
        //expected[7*5+3] = Spot::Shaded;
        // tempo
        expected[5 * 10 + 5] = Spot::Opaque;
        expected[5 * 10 + 6] = Spot::Opaque;
        expected[6 * 10 + 5] = Spot::Opaque;
        //shades
        expected[5 * 10 + 8] = Spot::Shaded;
        expected[5 * 10 + 9] = Spot::Shaded;
        expected[6 * 10 + 7] = Spot::Shaded;
        expected[6 * 10 + 8] = Spot::Shaded;
        expected[6 * 10 + 9] = Spot::Shaded;
        expected[7 * 10 + 6] = Spot::Shaded;
        expected[7 * 10 + 7] = Spot::Shaded;
        expected[7 * 10 + 8] = Spot::Shaded;
        expected[7 * 10 + 9] = Spot::Shaded;
        expected[8 * 10 + 5] = Spot::Shaded;
        expected[8 * 10 + 6] = Spot::Shaded;
        expected[8 * 10 + 7] = Spot::Shaded;
        expected[8 * 10 + 8] = Spot::Shaded;
        expected[8 * 10 + 9] = Spot::Shaded;
        expected[9 * 10 + 5] = Spot::Shaded;
        expected[9 * 10 + 6] = Spot::Shaded;
        expected[9 * 10 + 7] = Spot::Shaded;
        expected[9 * 10 + 8] = Spot::Shaded;
        expected[9 * 10 + 9] = Spot::Shaded;
        println!("{}", spot_grid_2d_to_string(&expected, 10, 10));
        let result = occlusion_map(vec![(2, 1)], vec![vec![(4, 4), (8, 4), (4, 8)]], 10, 10);
        if result == expected {
            Ok(())
        } else {
            println!("{}", spot_grid_2d_to_string(&result, 10, 10));
            Err(String::from(format!("{:?}\n{:?}", expected, result)))
        }
    }
}
