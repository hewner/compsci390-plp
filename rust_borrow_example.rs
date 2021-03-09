pub struct Person {
    first : String,
    last : String
}


fn main() {
    let mut me = Person { first : "Mike".to_string() , last : "Hewner".to_string() };

    let first_name = me.first.clone();
    update_name(&mut me, &first_name);
    
    println!("A person: {} {}", &me.first, &me.last);
}

fn update_name(person : &mut Person, to_append : & str) {
    person.last.push_str(to_append);
}

