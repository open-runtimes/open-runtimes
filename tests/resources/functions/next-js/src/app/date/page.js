export const dynamic = 'force-dynamic';

export default function Page() {
  const date = new Date().toISOString();

  return (
    <p id="date">{date}</p>
  );
}