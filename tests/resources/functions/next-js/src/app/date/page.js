export const dynamic = 'force-dynamic';

export default function Page() {
  const date = new Date().toISOString();

  return (
    <p>[DATE_START]{date}[DATE_END]</p>
  );
}