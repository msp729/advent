use std::{
    fs::File,
    io::{BufRead, BufReader, Result},
};

type Size = u32;
type ID = i64;
type FS = Vec<(ID, Size)>;
type BLK = Vec<ID>;

fn elab(f: &FS) -> BLK {
    let rsize = f.iter().map(|x| x.1).sum::<u32>() as usize;
    let mut ret = Vec::with_capacity(rsize);
    for (id, size) in f {
        for _ in 0..*size {
            ret.push(*id)
        }
    }
    ret
}

fn show(fs: &FS) -> String {
    let rsize = fs.iter().map(|x| x.1).sum::<u32>() as usize;
    let mut ret = vec!['.'; rsize];
    let mut pos = 0;
    for (id, size) in fs {
        if let Ok(id) = u32::try_from(*id) {
            for i in pos..pos + *size as usize {
                ret[i] = char::from_digit(id, 36).unwrap_or('?');
            }
        }
        pos += *size as usize
    }
    String::from_iter(ret)
}

fn parseout(f: File) -> Result<FS> {
    let mut f = BufReader::new(f);
    let mut id = 0;
    let mut file = true;
    let mut fs = Vec::new();
    loop {
        let b = f.fill_buf()?;
        if let Some(c) = b.get(0) {
            if let Some(size) = (*c as char).to_digit(10) {
                if file {
                    fs.push((id, size));
                } else {
                    fs.push((-1, size));
                    id += 1;
                }
                file = !file;
            }
            f.consume(1);
        } else {
            break Ok(fs);
        }
    }
}

fn getinput() -> Result<FS> {
    parseout(File::open("../inputs/9.txt")?)
}

fn getsample() -> Result<FS> {
    parseout(File::open("../samples/9.txt")?)
}

// this doesn't work, but the Haskell solution for part 1 works, so it's fine.
fn condense1(fs: &mut FS) {
    let mut lp = 0;
    let mut rp = fs.len() - 1;
    while lp < rp {
        if fs[lp].0 != -1 {
            lp += 1;
        } else if fs[rp].0 == -1 {
            rp -= 1;
        } else if fs[lp].1 > fs[rp].1 {
            fs.insert(lp + 1, (-1, fs[lp].1 - fs[rp].1));
            fs[lp] = fs[rp];
            fs[rp].0 = -1;
        } else if fs[lp].1 < fs[rp].1 {
            fs[lp].0 = fs[rp].0;
            fs[rp].1 -= fs[lp].1;
            fs.insert(rp - 1, (-1, fs[lp].1));
        } else {
            fs[lp] = fs[rp];
            fs[rp].0 = -1;
        }
    }
}

fn condense2(fs: &mut FS) {
    let mut id = fs.iter().map(|x| x.0).max().unwrap();
    let mut rp = fs.len() - 1;
    while id > 0 {
        while fs[rp].0 != id {
            rp -= 1;
        }
        let mut lp = 0;
        let size = fs[rp].1;
        while lp < rp {
            if fs[lp].0 == -1 && fs[lp].1 >= size {
                fs[lp].0 = fs[rp].0;
                fs[rp].0 = -1;
                if fs[lp].1 > size {
                    fs.insert(lp + 1, (-1, fs[lp].1 - size));
                    fs[lp].1 = size;
                }
                break;
            }
            lp += 1;
        }
        id -= 1;
    }
}

fn cksum(fs: &FS) -> usize {
    let mut pos = 0;
    let mut run = 0;
    for block in fs {
        let size = block.1 as usize;
        if let Ok(id) = usize::try_from(block.0) {
            run += id * (size * pos + (size * (size - 1)) / 2);
        }
        pos += size;
    }
    run
}

fn ck2(blk: &BLK) -> usize {
    blk.iter()
        .zip(0..)
        .filter_map(|(a, b)| Some((usize::try_from(*a).ok()?, b)))
        .map(|(a, b)| a * b)
        .sum()
}

pub fn main() {
    let mut inp = getinput().expect("Couldn't prepare input");
    condense2(&mut inp);
    println!("{}", cksum(&inp));
}
/*
 */
