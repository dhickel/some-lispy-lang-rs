use lang::util;


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HeapItem {
    pub typ: u16,
    pub size: usize,
    pub loc: *mut u8,
    active: bool,
}


#[derive(Debug)]
pub struct Heap {
    items: Vec<HeapItem>,
    data_start: *mut u8,
    data_top: *mut u8,
    data_capacity: usize,
    free_blocks: Vec<(usize, *mut u8)>,
    free_indexes: Vec<usize>,
}


impl Heap {
    pub fn new(size: usize) -> Heap {
        let heap = util::get_byte_array(size);
        let mut heap_struct = Heap {
            items: Vec::<HeapItem>::with_capacity(100),
            data_start: heap,
            data_top: heap,
            data_capacity: size,
            free_blocks: Vec::<(usize, *mut u8)>::with_capacity(100),
            free_indexes: Vec::<usize>::with_capacity(100),
        };

        let nil_arr: [u8; 8] = [0; 8];
        unsafe { heap_struct.insert_bytes(nil_arr.as_ptr(), 8, 0); }
        heap_struct
    }


    pub fn get_nil_obj(&self) -> u64 {
        0_u64
    }


    pub fn insert_item(&mut self, item: &[u8], typ: u16) -> u64 {
        unsafe {
            let loc = self.get_insert_loc(item.len());
            std::ptr::copy_nonoverlapping(item.as_ptr(), loc, item.len());

            let tag = HeapItem {
                typ,
                size: item.len(),
                loc,
                active: true,
            };

            let index = self.get_index();
            self.items.insert(index, tag);
            index as u64
        }
    }

// pub fn allocate_full_pair(&mut self, car: HeapItem, cdr_tag: HeapItem) -> HeapItem {
//     
// }
// 
// pub fn allocate_end_pair(&mut self, car: [u8, 8]) -> u64 {}

    pub unsafe fn insert_bytes(&mut self, ptr: *const u8, size: usize, typ: u16) -> u64 {
        let loc = self.get_insert_loc(size);
        std::ptr::copy_nonoverlapping(ptr, loc, size);

        let item = HeapItem {
            typ,
            size,
            loc,
            active: true,
        };

        let index = self.get_index();
        self.items.insert(index, item);
        index as u64
    }

    pub unsafe fn insert_pair(&mut self, car_ptr: *const u8, cdr_ptr: *const u8, size: usize, typ: u16) -> u64 {
        let loc = self.get_insert_loc(size * 2);
        std::ptr::copy_nonoverlapping(car_ptr, loc, size);
        std::ptr::copy_nonoverlapping(cdr_ptr, loc.add(size), size);
        
        let item = HeapItem {
            typ,
            size: size * 2,
            loc,
            active: true,
        };
        
        let index = self.get_index();
        self.items.insert(index, item);
        index as u64
    }


    pub fn get_item(&self, index: u64) -> (*const u8, usize) {
        let meta = self.items[index as usize];
        unsafe {
            (meta.loc, meta.size)
        }
    }


    pub fn get_item_meta(&self, index: u64) -> &HeapItem {
        unsafe { self.items.get_unchecked(index as usize) }
    }


    fn get_index(&mut self) -> usize {
        if !self.free_indexes.is_empty() {
            self.free_indexes.pop().unwrap()
        } else { self.items.len() }
    }


    fn get_insert_loc(&mut self, size: usize) -> *mut u8 {
        let mut loc = None;
        let mut loc_size = usize::MAX;
        let mut idx: usize = 0;

        for i in 0..self.free_blocks.len() {
            let block = &self.free_blocks[i];
            if block.0 >= size && block.0 - size < loc_size {
                loc = Some(block.1);
                loc_size = block.0;
                idx = i;
            }
        }
        if let Some(pointer) = loc {
            self.free_blocks.swap_remove(idx);
            pointer
        } else {
            if self.data_capacity < size { panic!("Heap overflow") }
            let loc = self.data_top;
            self.data_capacity -= size;
            unsafe { self.data_top = loc.add(size); }
            loc
        }
    }
}