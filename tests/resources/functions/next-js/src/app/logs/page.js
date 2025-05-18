export const dynamic = 'force-dynamic';

export default function Page() {
  console.log("A log printed");
  console.error("An error printed");
  
  const msg = "All logs printed";

  return (
    <p>{msg}</p>
  );
}
