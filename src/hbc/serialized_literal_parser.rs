use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TagType {
    NullTag = 0,
    TrueTag = 1,
    FalseTag = 2,
    NumberTag = 3,
    LongStringTag = 4,
    ShortStringTag = 5,
    ByteStringTag = 6,
    IntegerTag = 7,
}

impl TagType {
    pub fn from_u8(byte: u8) -> Option<Self> {
        use TagType::*;
        Some(match byte {
            0 => NullTag,
            1 => TrueTag,
            2 => FalseTag,
            3 => NumberTag,
            4 => LongStringTag,
            5 => ShortStringTag,
            6 => ByteStringTag,
            7 => IntegerTag,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone)]
pub enum SLPValue {
    Null,
    True,
    False,
    Number(f64),
    LongString(u32),
    ShortString(u16),
    ByteString(u8),
    Integer(i32),
}

impl SLPValue {
    pub fn tag_type(&self) -> TagType {
        use SLPValue::*;
        match self {
            Null => TagType::NullTag,
            True => TagType::TrueTag,
            False => TagType::FalseTag,
            Number(_) => TagType::NumberTag,
            LongString(_) => TagType::LongStringTag,
            ShortString(_) => TagType::ShortStringTag,
            ByteString(_) => TagType::ByteStringTag,
            Integer(_) => TagType::IntegerTag,
        }
    }

    pub fn to_string(&self, string_table: &[String]) -> String {
        use SLPValue::*;
        match self {
            Null => "null".to_string(),
            True => "true".to_string(),
            False => "false".to_string(),
            Number(n) => n.to_string(),
            LongString(idx) => {
                if *idx as usize >= string_table.len() {
                    format!("string_table[{}] (out of bounds)", idx)
                } else {
                    string_table[*idx as usize].clone()
                }
            }
            ShortString(idx) => {
                if *idx as usize >= string_table.len() {
                    format!("string_table[{}] (out of bounds)", idx)
                } else {
                    string_table[*idx as usize].clone()
                }
            }
            ByteString(idx) => {
                if *idx as usize >= string_table.len() {
                    format!("string_table[{}] (out of bounds)", idx)
                } else {
                    string_table[*idx as usize].clone()
                }
            }
            Integer(i) => i.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SLPArray {
    pub items: Vec<SLPValue>,
}

impl SLPArray {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn to_strings(&self, string_table: &[String]) -> Vec<String> {
        self.items.iter().map(|item| item.to_string(string_table)).collect()
    }
}

impl Serialize for SLPArray {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.items.len()))?;
        for item in &self.items {
            // Show actual values instead of trying to resolve string table
            match item {
                SLPValue::LongString(idx) => seq.serialize_element(&format!("string[{}]", idx))?,
                SLPValue::ShortString(idx) => seq.serialize_element(&format!("string[{}]", idx))?,
                SLPValue::ByteString(idx) => seq.serialize_element(&format!("string[{}]", idx))?,
                _ => seq.serialize_element(&item.to_string(&[]))?,
            }
        }
        seq.end()
    }
}

// Custom serialization wrapper that includes string table
pub struct SLPArrayWithStrings<'a> {
    pub array: &'a SLPArray,
    pub string_table: &'a [String],
}

impl<'a> Serialize for SLPArrayWithStrings<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.array.items.len()))?;
        for item in &self.array.items {
            seq.serialize_element(&item.to_string(self.string_table))?;
        }
        seq.end()
    }
}

pub fn unpack_slp_array(data: &[u8], num_items: Option<usize>) -> Result<SLPArray, String> {
    let mut items = Vec::new();
    let mut offset = 0;

    while num_items.map_or(true, |n| items.len() < n) {
        // eprintln!("Start of loop ================================");
        if offset >= data.len() {
            break;
        }

        let tag_byte = data[offset];
        // println!("tag_byte: {:b} offset: {}", tag_byte, offset);
        offset += 1;

        let (tag_type, length) = if (tag_byte & 0x80) != 0 {
            // Extended format: 1 ttt llll, llll llll
            if offset >= data.len() {
                break;
            }
            let length_byte = data[offset];
            // println!("length_byte: {:?} offset: {}", length_byte, offset);
            offset += 1;
            // eprintln!("\n\nEXTENDED FORMAT");
            // eprintln!("1 ttt llll,llllllll");
            // eprintln!("offset: {}", offset-2);
            // eprintln!("tag_byte: {:?} (0b{:b})", tag_byte, tag_byte);
            // eprintln!("length_byte: {:?} (0b{:b})", length_byte, length_byte);
            // eprintln!("tag_type (>> 4 & 0x07): {:?} (0b{:b})", (tag_byte >> 4) & 0x07, (tag_byte >> 4) & 0x07);
            // eprintln!("tag_byte & 0x0F: {:?} (0b{:b})", tag_byte & 0x0F, tag_byte & 0x0F);
            // eprintln!("((tag_byte & 0x0F) as usize) << 8: {:?}", ((tag_byte & 0x0F) as usize) << 8);
            // eprintln!("((tag_byte & 0x0F) as usize) << 8 | (length_byte as usize): {:?}", ((tag_byte & 0x0F) as usize) << 8 | (length_byte as usize));
            // eprintln!("length_byte as usize: {:?}", length_byte as usize);
            
            let tag_type = TagType::from_u8((tag_byte >> 4) & 0x07)
                .ok_or_else(|| format!("Unknown tag type: {}", (tag_byte >> 4) & 0x07))?;
            let length = (((tag_byte & 0x0F) as usize) << 8) | (length_byte as usize);

            if tag_type == TagType::LongStringTag {
                // eprintln!("long string tag, length: {}", length);
                offset -= 1;
            }

            // eprintln!("tag_type: {:?} length: {}", tag_type, length);
            (tag_type, length)
        } else {
            // Simple format: 0 ttt llll

            // eprintln!("\n\nSIMPLE FORMAT");
            // eprintln!("0 ttt llll");
            // eprintln!("offset: {}", offset-1);
            // eprintln!("tag_byte: {:?} (0b{:b})", tag_byte, tag_byte);
            // eprintln!("tag_type (>> 4 & 0x07): {:?} (0b{:b})", (tag_byte >> 4) & 0x07, (tag_byte >> 4) & 0x07);
            // eprintln!("tag_byte & 0x0F: {:?} (0b{:b})", tag_byte & 0x0F, tag_byte & 0x0F);

            let tag_type = TagType::from_u8((tag_byte >> 4) & 0x07)
                .ok_or_else(|| format!("Unknown tag type: {}", (tag_byte >> 4) & 0x07))?;
            let length = (tag_byte & 0x0F) as usize;

            // eprintln!("tag_type: {:?} length: {}", tag_type, length);
            (tag_type, length)
        };

        // println!("length: {}", length);
        for _ in 0..length {
            if num_items.map_or(false, |n| items.len() >= n) {
                break;
            }

            match tag_type {
                TagType::NullTag => {
                    items.push(SLPValue::Null)
                },
                TagType::TrueTag => items.push(SLPValue::True),
                TagType::FalseTag => items.push(SLPValue::False),
                TagType::NumberTag => {
                    if offset + 8 > data.len() {
                        // eprintln!("Error parsing serialized literal: Not enough bytes for double (offset={}, data.len={})", offset, data.len());
                        // eprintln!("tag_type: {:?} (0b{:b})", tag_type, tag_type as u8);
                        // eprintln!("tag_byte: {:?} (0b{:b})", tag_byte, tag_byte);
                        // eprintln!("length: {}", length);
                        // eprintln!("offset: {}", offset);
                        // eprintln!("data.len: {}", data.len());
                        // eprintln!("data[offset..offset + 8]: {:?}", &data[offset..data.len()]);
                        // return Err(format!("Not enough bytes for double (offset={}, data.len={})", offset, data.len()));
                        
                        offset += 8;
                        // eprintln!("BREAKING OUT OF LOOP");
                        break;
                    }
                    let bytes = &data[offset..offset + 8];
                    let val = f64::from_le_bytes(bytes.try_into().unwrap());
                    // eprintln!("read double bytes: {:?}, offset: {}, val: {}", &data[offset..offset + 8], offset, val);
                    offset += 8;
                    items.push(SLPValue::Number(val));
                }
                TagType::LongStringTag => {
                    if offset + 4 > data.len() {
                        return Err("Not enough bytes for long string".to_string());
                    }
                    let bytes = &data[offset..offset + 4];
                    let val = u32::from_le_bytes(bytes.try_into().unwrap());

                    // eprintln!("read long string bytes: {:?}, offset: {}, val: {}", &data[offset..offset + 4], offset, val);
                    
                    offset += 4;
                    items.push(SLPValue::LongString(val));
                }
                TagType::ShortStringTag => {
                    if offset + 2 > data.len() {
                        return Err("Not enough bytes for short string".to_string());
                    }
                    let bytes = &data[offset..offset + 2];
                    let val = u16::from_le_bytes(bytes.try_into().unwrap());
                    // eprintln!("read short string offset: {}, val: {}", offset, val);

                    offset += 2;
                    items.push(SLPValue::ShortString(val));
                }
                TagType::ByteStringTag => {
                    if offset >= data.len() {
                        return Err("Not enough bytes for byte string".to_string());
                    }
                    let val = data[offset];
                    // eprintln!("read byte string offset: {}, val: {}", offset, val);
                    offset += 1;
                    items.push(SLPValue::ByteString(val));
                }
                TagType::IntegerTag => {
                    if offset + 4 > data.len() {
                        return Err("Not enough bytes for integer".to_string());
                    }
                    let bytes = &data[offset..offset + 4];
                    let val = i32::from_le_bytes(bytes.try_into().unwrap());

                    // eprintln!("read integer offset: {}, val: {} bytes: {:?}", offset, val, bytes.iter().map(|b| format!("{:08b}", b)).collect::<Vec<String>>().join(" "));
                    offset += 4;
                    items.push(SLPValue::Integer(val));
                }
            }
        }

        // eprintln!("after loop, offset: {}, items.len: {}", offset, items.len());
    }

    Ok(SLPArray { items: if let Some(n) = num_items { items.into_iter().take(n).collect() } else { items } })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unpack_slp_array_simple() {
        // Test data: [null, true, false, 42, "hello"]
        // Format: tag byte (type << 4 | length), then values
        let data = vec![
            0x01, // null tag (0), length 1
            0x11, // true tag (1), length 1  
            0x21, // false tag (2), length 1
            0x31, // number tag (3), length 1
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x45, 0x40, // 42.0 as f64
            0x51, // short string tag (5), length 1
            0x05, 0x00, // string index 5
        ];
        
        let result = unpack_slp_array(&data, None).unwrap();
        assert_eq!(result.items.len(), 5);
        
        match &result.items[0] {
            SLPValue::Null => {},
            _ => panic!("Expected null"),
        }
        
        match &result.items[1] {
            SLPValue::True => {},
            _ => panic!("Expected true"),
        }
        
        match &result.items[2] {
            SLPValue::False => {},
            _ => panic!("Expected false"),
        }
        
        match &result.items[3] {
            SLPValue::Number(n) => assert_eq!(*n, 42.0),
            _ => panic!("Expected number"),
        }
        
        match &result.items[4] {
            SLPValue::ShortString(idx) => assert_eq!(*idx, 5),
            _ => panic!("Expected short string"),
        }
    }
} 