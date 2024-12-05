use std::iter::Peekable;
use std::slice::Iter;
use itertools::Itertools;
use nom::character::complete::hex_digit0;

fn parse_input(line: &str) -> Vec<u8> {
    line
        .bytes()
        .map(|x| hex_digits.find(x).unwrap())
        .chunks(2)
        .map(|c| c.0 * 16 + c.1)
        .collect()
    // line.chars().chunks(2).map(|s| s.parse().unwrap()).collect()
}


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

fn main() {
    // let input = include_str!("sample_input.txt");
    let input = include_str!("input.txt");
    let lines = input.lines();

    for line in lines {
        let bits = hex_to_bits(line);
        print!("{} ", line);

        let (_, packet) = parse_packet(&bits);
        println!("version sum {}", version_sum(&packet));
        assert_eq!(version_sum(&packet), 883);

        dbg!(&packet);
        println!("value {}", packet_value(&packet));
        assert_eq!(packet_value(&packet), 1675198555015);
    }
}

fn version_sum(packet: &Packet) -> usize {
    packet.version + packet.sub_packets.iter().map(version_sum).sum::<usize>()
}

fn packet_value(packet: &Packet) -> usize {
    match packet.type_ {
        PacketType::Sum => packet.sub_packets.iter().map(packet_value).sum::<usize>(),
        PacketType::Product => packet.sub_packets.iter().map(packet_value).product::<usize>(),
        PacketType::Minimum => packet.sub_packets.iter().map(packet_value).reduce(|acc, v| acc.min(v)).unwrap(),
        PacketType::Maximum => packet.sub_packets.iter().map(packet_value).reduce(|acc, v| acc.max(v)).unwrap(),
        PacketType::Literal => packet.literal,
        PacketType::GreaterThan => usize::from(packet_value(&packet.sub_packets[0]) > packet_value(&packet.sub_packets[1])),
        PacketType::LessThan => usize::from(packet_value(&packet.sub_packets[0]) < packet_value(&packet.sub_packets[1])),
        PacketType::EqualTo => usize::from(packet_value(&packet.sub_packets[0]) == packet_value(&packet.sub_packets[1])),
    }
}

fn take_bits(bits: &[u8], n: usize) -> (&[u8], usize) {
    (&bits[n..], bits[0..n].iter().fold(0, |acc, &v| (acc << 1) + usize::from(v)))
}

fn take_literal(bits: &[u8]) -> (&[u8], usize) {
    let (mut bits, mut tagged_byte) = take_bits(bits, 5);
    // let mut tagged_byte = take_bits(bits, 5);
    let mut n = tagged_byte & 0x0f;
    while tagged_byte & 0x10 != 0 {
        let res = take_bits(bits, 5);

        bits = res.0;
        tagged_byte = res.1;
        n = (n << 4) + (tagged_byte & 0x0f);
    }
    (bits, n)
}

#[derive(Debug, PartialEq)]
enum PacketType {
    Sum,
    Product,
    Minimum,
    Maximum,
    GreaterThan,
    LessThan,
    EqualTo,
    Literal,
}

#[derive(Debug)]
struct Packet {
    version: usize,
    type_: PacketType,
    literal: usize,
    sub_packets: Vec<Packet>,
}

fn parse_type(bits: &[u8]) -> (&[u8], PacketType) {
    match take_bits(bits, 3) {
        (bits, 0) => (bits, PacketType::Sum),
        (bits, 1) => (bits, PacketType::Product),
        (bits, 2) => (bits, PacketType::Minimum),
        (bits, 3) => (bits, PacketType::Maximum),
        (bits, 4) => (bits, PacketType::Literal),
        (bits, 5) => (bits, PacketType::GreaterThan),
        (bits, 6) => (bits, PacketType::LessThan),
        (bits, 7) => (bits, PacketType::EqualTo),
        _ => panic!(),
    }
}

fn parse_packet(bits: &[u8]) -> (&[u8], Packet) {
    let (bits, version) = take_bits(bits, 3);
    let (bits, type_) = parse_type(bits);
    if type_ == PacketType::Literal {
        // literal
        let (bits, literal) = take_literal(bits);
        return (bits, Packet { version, type_, literal, sub_packets: Vec::new() });
    }

    let (bits, len_type) = take_bits(bits, 1);
    let (bits, sub_packets) = if len_type == 0 {
        let (bits, len_subpackets) = take_bits(bits, 15);
        let (bits, subpacket_bits) = (&bits[len_subpackets..], &bits[..len_subpackets]);
        let (_, packets) = take_packets(&subpacket_bits);
        (bits, packets)
    } else {
        let (bits, num_subpackets) = take_bits(bits, 11);
        let mut packets: Vec<Packet> = Vec::new();
        let mut bits = bits;
        for _ in 0..num_subpackets {
            let (b, packet) = parse_packet(bits);
            bits = b;
            packets.push(packet);
        }
        (bits, packets)
    };
    (bits, Packet { version, type_, literal: 0, sub_packets })
}

fn take_packets(bits: &[u8]) -> (&[u8], Vec<Packet>) {
    let mut packets: Vec<Packet> = Vec::new();
    let mut bits = bits;
    while bits.len() > 0 {
        let (b, packet) = parse_packet(bits);
        bits = b;
        packets.push(packet);
    }
    (bits, packets)
}
