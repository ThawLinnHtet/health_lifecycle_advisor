import { AdvisorForm } from "@/components/AdvisorForm";

export default function Home() {
  return (
    <main className="min-h-screen bg-black selection:bg-emerald-500/30 text-white flex flex-col relative overflow-hidden">
      {/* Background Gradients */}
      <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[1000px] h-[500px] opacity-20 pointer-events-none">
        <div className="absolute inset-0 bg-gradient-to-br from-emerald-500/30 via-emerald-800/20 to-transparent blur-[100px] rounded-full mix-blend-screen" />
      </div>

      <div className="relative z-10 flex flex-col flex-1 items-center justify-center p-6 md:p-12 lg:p-24 w-full">
        <div className="max-w-4xl w-full space-y-12">

          <div className="flex flex-col items-center text-center space-y-6">
            <div className="inline-flex items-center justify-center rounded-full border border-emerald-500/30 bg-emerald-500/10 px-3 py-1 text-sm font-medium text-emerald-300 mb-4 backdrop-blur-sm">
              <span className="flex h-2 w-2 rounded-full bg-emerald-500 mr-2 animate-pulse"></span>
              Powered by SWI-Prolog Engine
            </div>
            <h1 className="text-5xl md:text-7xl font-semibold tracking-tight leading-[1.1] text-transparent bg-clip-text bg-gradient-to-b from-white to-white/60">
              Unlock Your <br className="hidden md:block" /> Health Potential
            </h1>
            <p className="max-w-[600px] text-zinc-400 md:text-xl text-lg font-light">
              Experience an intelligent advisor that adapts to your unique lifecycle stage, delivering precise, actionable insights.
            </p>
          </div>

          <AdvisorForm />

        </div>
      </div>

      {/* Footer */}
      <div className="relative z-10 py-6 text-center text-zinc-600 text-sm">
        Aura Diagnostics &copy; 2024
      </div>
    </main>
  );
}
