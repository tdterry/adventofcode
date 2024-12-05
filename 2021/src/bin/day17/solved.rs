use std::slice::Iter;

fn hex_to_bits(line: &str) -> Vec<u8> {
    let hex_digits = "0123456789ABCDEF";
    let mut ret: Vec<u8> = vec![];
    for c in line.chars() {
        let n = hex_digits.find(c).unwrap();
        for i in 0..4 {
            ret.push(if (n & 1 << (3 - i)) != 0 { 1 } else { 0 });
        }
    }
    ret
}

fn print_bits(bits: &[u8]) {
    for b in bits {
        print!("{}", b);
    }
    println!();
}

fn main() {
    // let input = include_str!("sample_input2.txt");
    let input = include_str!("input.txt");
    let mut lines = input.lines();

    for line in lines {
        let bits = hex_to_bits(line);
        print!("{} ", line);
        print_bits(&bits);

        let packet = take_packet(&mut bits.iter());
        println!("version sum {}", version_sum(&packet));

        dbg!(&packet);
        println!("value {}", packet_value(&packet));
    }
}

fn version_sum(packet: &Packet) -> usize {
    packet.version + packet.sub_packets.iter().map(version_sum).sum::<usize>()
}

fn packet_value(packet: &Packet) -> usize {
    match packet.type_ {
        0 => packet.sub_packets.iter().map(packet_value).sum::<usize>(),
        1 => packet.sub_packets.iter().map(packet_value).product::<usize>(),
        2 => packet.sub_packets.iter().map(packet_value).reduce(|acc, v| acc.min(v)).unwrap(),
        3 => packet.sub_packets.iter().map(packet_value).reduce(|acc, v| acc.max(v)).unwrap(),
        4 => packet.literal,
        5 => usize::from(packet_value(&packet.sub_packets[0]) > packet_value(&packet.sub_packets[1])),
        6 => usize::from(packet_value(&packet.sub_packets[0]) < packet_value(&packet.sub_packets[1])),
        7 => usize::from(packet_value(&packet.sub_packets[0]) == packet_value(&packet.sub_packets[1])),
        _ => 0,
    }
}

fn take_bits(bits: &mut Iter<u8>, n: usize) -> usize {
    let ret = bits.take(n).fold(0, |acc, &v| (acc << 1) + usize::from(v));
    // println!("took {} = {}", n, ret);
    ret
}

fn take_literal(bits: &mut Iter<u8>) -> usize {
    let mut n = 0;
    let mut tagged_byte = take_bits(bits, 5);
    n = tagged_byte & 0x0f;
    while tagged_byte & 0x10 != 0 {
        tagged_byte = take_bits(bits, 5);
        n = (n << 4) + (tagged_byte & 0x0f);
    }
    // println!("literal {}", n);
    n
}

#[derive(Default, Debug)]
struct Packet {
    version: usize,
    type_: usize,
    literal: usize,
    sub_packets: Vec<Packet>,
}

fn take_packet(bits: &mut Iter<u8>) -> Packet {
    let mut packet = Packet::default();
    packet.version = take_bits(bits, 3);
    packet.type_ = take_bits(bits, 3);
    if packet.type_ == 4 {
        // literal
        packet.literal = take_literal(bits);
    } else {
        let len_type = {
            bits.take(1).map(|&x| x).next().unwrap()
        };
        if len_type == 0 {
            let len_subpackets = take_bits(bits, 15);
            let subpacket_bits: Vec<_> = bits.take(len_subpackets).map(|&x| x).collect();
            packet.sub_packets = take_packets(&mut subpacket_bits.iter());
            println!("len sub-packets {}", len_subpackets);
        } else {
            let num_subpackets = take_bits(bits, 11);
            for _ in 0..num_subpackets {
                packet.sub_packets.push(take_packet(bits));
            }
            println!("num sub-packets {}", num_subpackets);
        }
    }
    packet
}

fn take_packets(bits: &mut Iter<u8>) -> Vec<Packet> {
    let mut packets: Vec<Packet> = Vec::new();
    loop {
        let packet = take_packet(bits);
        packets.push(packet);
        if bits.clone().next().is_none() {
            break
        }
    }
    packets
}
