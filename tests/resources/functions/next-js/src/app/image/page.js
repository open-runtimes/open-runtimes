import Image from "next/image";

export default function Page() {
    const images = [
        "https://images.unsplash.com/photo-1506744038136-46273834b3fb",
        "https://cdn.pixabay.com/photo/2020/11/10/01/34/pet-5728249_1280.jpg",
    ];

    return (
        <div>
            {images.map((src, idx) => (
                <div key={idx}>
                    <Image
                        width="400"
                        height="400"
                        src={src}
                    />
                </div>
            ))}
        </div>
    );
}