import Image from "next/image";

export default function Page() {
  const images = [
    "https://images.unsplash.com/photo-1506744038136-46273834b3fb?auto=format&fit=crop&w=400&q=80",
    "https://images.unsplash.com/photo-1465101046530-73398c7f28ca?auto=format&fit=crop&w=400&q=80",
    "https://images.unsplash.com/photo-1519125323398-675f0ddb6308?auto=format&fit=crop&w=400&q=80",
    "https://images.unsplash.com/photo-1529626455594-4ff0802cfb7e?auto=format&fit=crop&w=400&q=80",
    "https://images.unsplash.com/photo-1519985176271-adb1088fa94c?auto=format&fit=crop&w=400&q=80",
  ];

  return (
    <div className="min-h-screen flex flex-col items-center justify-center p-8 bg-gray-50 dark:bg-gray-900">
      <h1 className="text-2xl font-bold mb-8 text-center">Demo: Sample Images from External Links</h1>
      <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 gap-6 w-full max-w-4xl">
        {images.map((src, idx) => (
          <div key={idx} className="rounded-lg overflow-hidden shadow-md bg-white dark:bg-gray-800 flex items-center justify-center">
            <Image
              src={src}
              alt={`Sample ${idx + 1}`}
              className="object-cover w-full h-64 transition-transform duration-200 hover:scale-105"
              loading="lazy"
              width={400}
              height={400}
            />
          </div>
        ))}
      </div>
    </div>
  );
}