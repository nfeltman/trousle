mod nsf;

fn main() {
    println!("hello world");
    println!("{}", std::mem::size_of::<nsf::NsfHeader>());
}