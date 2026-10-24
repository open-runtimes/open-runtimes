import json from './.config/.file.json';

export const dynamic = 'force-dynamic';

export default function Page() {
  const value = json.value;

  return (
    <p>{value}</p>
  );
}