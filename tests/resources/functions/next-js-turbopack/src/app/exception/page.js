export const dynamic = 'force-dynamic';

export default function Page() {
  throw new Error('Code exception occured');
  const msg = "No exceptions";

  return (
    <p>{msg}</p>
  );
}