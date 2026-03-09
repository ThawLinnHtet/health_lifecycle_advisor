"use client";

import { useState, useEffect, useRef } from "react";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import * as z from "zod";
import { Activity, HeartPulse, Moon, Brain, ChevronRight, ChevronLeft, History, X, Clock } from "lucide-react";

import { Button } from "@/components/ui/button";
import {
    Form,
    FormControl,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from "@/components/ui/select";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Slider } from "@/components/ui/slider";
import { Checkbox } from "@/components/ui/checkbox";

// ─── Constants ───────────────────────────────────────────────────────────────
const HEALTH_GOALS = [
    { id: "weight_loss", label: "Weight loss" },
    { id: "muscle_gain", label: "Muscle gain" },
    { id: "improve_sleep", label: "Improve sleep" },
    { id: "reduce_stress", label: "Reduce stress" },
    { id: "heart_health", label: "Improve heart health" },
    { id: "general_wellness", label: "General wellness" },
] as const;

const SYMPTOMS = [
    { id: "fatigue", label: "Frequent fatigue" },
    { id: "headaches", label: "Headaches" },
    { id: "back_pain", label: "Back pain" },
    { id: "poor_concentration", label: "Poor concentration" },
    { id: "shortness_breath", label: "Shortness of breath" },
] as const;

const STEP_TITLES = [
    { num: 1, title: "Basic Info", icon: "👤" },
    { num: 2, title: "Lifestyle", icon: "🏃" },
    { num: 3, title: "Goals", icon: "🎯" },
    { num: 4, title: "Symptoms", icon: "🩺" },
];

const ADVICE_CATEGORIES: { pattern: RegExp; icon: string; label: string; color: string }[] = [
    { pattern: /burnout|stress|cortisol|mindfulness|breathing|mental/i, icon: "🧠", label: "Mental", color: "text-purple-400" },
    { pattern: /sleep|bed|wind-down|insomnia|rest/i, icon: "💤", label: "Sleep", color: "text-blue-400" },
    { pattern: /bmi|weight|calori|fat|nutriti|diet|meal|sugar|protein|food|iron|folate|cholesterol/i, icon: "🍎", label: "Diet", color: "text-amber-400" },
    { pattern: /activity|exercise|walk|sedentary|muscle|training|aerobic|swim|core|bone|resistance/i, icon: "🏋️", label: "Activity", color: "text-green-400" },
    { pattern: /headache|breath|back pain|fatigue|concentration|symptom/i, icon: "⚕️", label: "Symptom", color: "text-red-400" },
    { pattern: /heart|cardiovascular|blood pressure/i, icon: "❤️", label: "Vital", color: "text-rose-400" },
    { pattern: /infant|child|teenager|senior|young adult|adult/i, icon: "🌱", label: "Stage", color: "text-teal-400" },
];

function categorizeAdvice(text: string) {
    for (const cat of ADVICE_CATEGORIES) {
        if (cat.pattern.test(text)) return cat;
    }
    return { icon: "💡", label: "Tip", color: "text-emerald-400" };
}

// ─── Schema ──────────────────────────────────────────────────────────────────
const formSchema = z.object({
    age: z.coerce.number().min(0).max(120),
    gender: z.enum(["male", "female"], { message: "Please select a gender." }),
    height: z.coerce.number().min(50).max(300, "Enter height in cm"),
    weight: z.coerce.number().min(20).max(500, "Enter weight in kg"),
    activityLevel: z.string().min(1, "Please select an activity level."),
    dietType: z.string().min(1, "Please select a diet type."),
    sleepHours: z.coerce.number().min(0).max(24),
    stressLevel: z.coerce.number().min(1).max(10).default(5),
    goals: z.array(z.string()).min(1, "Select at least one goal."),
    symptoms: z.array(z.string()).optional(),
});
type FormValues = z.infer<typeof formSchema>;

// Step field groups for per-step validation
const STEP_FIELDS: (keyof FormValues)[][] = [
    ["age", "gender", "height", "weight"],
    ["activityLevel", "dietType", "sleepHours", "stressLevel"],
    ["goals"],
    ["symptoms"],
];

// ─── Animated Score Ring ─────────────────────────────────────────────────────
function ScoreRing({ score, grade }: { score: number; grade: string }) {
    const [animatedScore, setAnimatedScore] = useState(0);
    const circumference = 2 * Math.PI * 54;
    const offset = circumference - (animatedScore / 100) * circumference;
    const gradeColor = grade === "A" ? "#22c55e" : grade === "B" ? "#eab308" : grade === "C" ? "#f97316" : "#ef4444";

    useEffect(() => {
        let frame: number;
        let start: number | null = null;
        const duration = 1200;
        const animate = (ts: number) => {
            if (!start) start = ts;
            const progress = Math.min((ts - start) / duration, 1);
            const eased = 1 - Math.pow(1 - progress, 3);
            setAnimatedScore(Math.round(eased * score));
            if (progress < 1) frame = requestAnimationFrame(animate);
        };
        frame = requestAnimationFrame(animate);
        return () => cancelAnimationFrame(frame);
    }, [score]);

    return (
        <div className="relative w-40 h-40 mx-auto">
            <svg viewBox="0 0 120 120" className="w-full h-full -rotate-90">
                <circle cx="60" cy="60" r="54" fill="none" stroke="rgba(255,255,255,0.06)" strokeWidth="8" />
                <circle cx="60" cy="60" r="54" fill="none" stroke={gradeColor} strokeWidth="8" strokeLinecap="round"
                    strokeDasharray={circumference} strokeDashoffset={offset}
                    style={{ transition: "stroke-dashoffset 0.1s linear" }} />
            </svg>
            <div className="absolute inset-0 flex flex-col items-center justify-center">
                <span className="text-4xl font-bold text-white">{animatedScore}</span>
                <span className="text-xs font-semibold tracking-widest mt-1 px-2 py-0.5 rounded-full"
                    style={{ backgroundColor: gradeColor + "22", color: gradeColor }}>
                    GRADE {grade}
                </span>
            </div>
        </div>
    );
}

// ─── BMI Gauge ───────────────────────────────────────────────────────────────
function BMIGauge({ bmi, category }: { bmi: number; category: string }) {
    const clampedBmi = Math.min(Math.max(bmi, 10), 45);
    const percent = ((clampedBmi - 10) / 35) * 100;
    return (
        <div className="space-y-2">
            <div className="flex items-center justify-between text-xs text-zinc-500">
                <span>10</span><span>18.5</span><span>25</span><span>30</span><span>45</span>
            </div>
            <div className="relative h-3 rounded-full overflow-hidden bg-zinc-800">
                <div className="absolute inset-0 flex">
                    <div className="h-full bg-blue-500/60" style={{ width: "24.3%" }} />
                    <div className="h-full bg-green-500/60" style={{ width: "18.6%" }} />
                    <div className="h-full bg-yellow-500/60" style={{ width: "14.3%" }} />
                    <div className="h-full bg-red-500/60" style={{ width: "42.8%" }} />
                </div>
                <div className="absolute top-0 h-full w-1 bg-white rounded-full shadow-[0_0_6px_rgba(255,255,255,0.8)] transition-all duration-1000"
                    style={{ left: `${percent}%` }} />
            </div>
            <div className="flex items-center justify-between">
                <span className="text-xs text-zinc-500">Under</span>
                <span className="text-sm font-semibold text-white">{bmi.toFixed(1)} <span className="text-zinc-400 text-xs ml-1">({category})</span></span>
                <span className="text-xs text-zinc-500">Obese</span>
            </div>
        </div>
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// MAIN COMPONENT
// ═════════════════════════════════════════════════════════════════════════════
export function AdvisorForm() {
    const [step, setStep] = useState(0);
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [result, setResult] = useState<any>(null);
    const [error, setError] = useState<string | null>(null);
    const [slideDir, setSlideDir] = useState<"left" | "right">("left");
    const [submitReady, setSubmitReady] = useState(false);

    useEffect(() => {
        if (step === 3) {
            setSubmitReady(false);
            const timer = setTimeout(() => setSubmitReady(true), 600);
            return () => clearTimeout(timer);
        }
    }, [step]);

    const form = useForm<FormValues>({
        resolver: zodResolver(formSchema) as any,
        defaultValues: { age: undefined, gender: undefined, height: undefined, weight: undefined, activityLevel: "", dietType: "", sleepHours: 7, stressLevel: 5, goals: [], symptoms: [] },
    });

    async function goNext() {
        const fields = STEP_FIELDS[step];
        const valid = await form.trigger(fields);
        if (!valid) return;
        setSlideDir("left");
        setStep((s) => Math.min(s + 1, 3));
    }
    function goBack() {
        setSlideDir("right");
        setStep((s) => Math.max(s - 1, 0));
    }

    async function onSubmit(values: FormValues) {
        setIsSubmitting(true);
        setError(null);
        try {
            const response = await fetch("/api/advice", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(values) });
            if (!response.ok) { const errBody = await response.json().catch(() => ({})); throw new Error(errBody.error || `Status ${response.status}`); }
            const adviceResult = await response.json();
            setResult(adviceResult);
        } catch (err: any) {
            console.error(err);
            setError(err.message || "Something went wrong.");
        } finally { setIsSubmitting(false); }
    }

    function resetForm() { setResult(null); setStep(0); setError(null); }

    // ─── RESULTS VIEW ───────────────────────────────────────────────────────
    if (result) {
        return (
            <div className="space-y-6 animate-in fade-in zoom-in-95 duration-500">
                <Card className="w-full max-w-2xl mx-auto border-emerald-500/20 bg-black/40 backdrop-blur-xl">
                    <CardHeader className="pb-4 border-b border-white/5">
                        <CardTitle className="text-2xl font-semibold flex items-center gap-2 text-emerald-400">
                            <HeartPulse className="h-6 w-6" /> Your Health Action Plan
                        </CardTitle>
                    </CardHeader>
                    <CardContent className="pt-6 space-y-8">
                        {/* Score + Stage row */}
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 items-center">
                            <ScoreRing score={result.score ?? 50} grade={result.grade ?? "C"} />
                            <div className="space-y-3">
                                <div className="flex items-center gap-2">
                                    <span className="text-zinc-500 text-sm font-medium w-24">Life Stage</span>
                                    <span className="text-emerald-300 bg-emerald-500/10 px-3 py-1 rounded-full text-sm font-semibold">{result.stage}</span>
                                </div>
                                <div className="flex items-start gap-2">
                                    <span className="text-zinc-500 text-sm font-medium w-24 shrink-0">Focus Area</span>
                                    <span className="text-zinc-200 text-sm">{result.focusArea}</span>
                                </div>
                            </div>
                        </div>

                        {/* BMI Gauge */}
                        {result.bmi > 0 && (
                            <div className="bg-zinc-900/40 border border-white/5 rounded-lg p-4">
                                <h4 className="text-sm font-medium text-zinc-400 mb-3">Body Mass Index</h4>
                                <BMIGauge bmi={result.bmi} category={result.bmiCategory} />
                            </div>
                        )}

                        {/* Advice Cards */}
                        <div>
                            <h4 className="text-emerald-400 font-medium mb-4 text-lg">Personalised Recommendations</h4>
                            <div className="grid gap-3">
                                {result.advice.map((item: string, i: number) => {
                                    const cat = categorizeAdvice(item);
                                    return (
                                        <div key={i} className="flex gap-3 bg-zinc-900/40 border border-white/5 rounded-lg p-4 hover:border-emerald-500/20 transition-all duration-300 group">
                                            <div className="flex flex-col items-center gap-1 shrink-0 pt-0.5">
                                                <span className="text-xl">{cat.icon}</span>
                                                <span className={`text-[10px] font-semibold uppercase tracking-wider ${cat.color}`}>{cat.label}</span>
                                            </div>
                                            <p className="text-zinc-300 text-sm leading-relaxed">{item}</p>
                                        </div>
                                    );
                                })}
                            </div>
                        </div>

                        {/* Actions */}
                        <div className="flex gap-3">
                            <Button variant="outline" className="w-full bg-transparent border-zinc-800 hover:bg-zinc-900 text-zinc-300 font-medium" onClick={resetForm}>
                                Analyze New Profile
                            </Button>
                        </div>
                    </CardContent>
                </Card>
            </div>
        );
    }

    // ─── WIZARD VIEW ─────────────────────────────────────────────────────────
    return (
        <Card className="w-full max-w-2xl mx-auto border-white/10 bg-black/40 backdrop-blur-xl shadow-2xl">
            <CardHeader className="space-y-4 pb-6">
                <div className="flex items-center justify-between">
                    <CardTitle className="text-3xl font-semibold tracking-tight text-white">Lifestyle Profile</CardTitle>
                </div>
                <CardDescription className="text-zinc-400">
                    Complete 4 sections for your health analysis.
                </CardDescription>

                {/* Progress Bar */}
                <div className="flex gap-2">
                    {STEP_TITLES.map((s, i) => (
                        <div key={i} className="flex-1 space-y-1">
                            <div className={`h-1.5 rounded-full transition-all duration-500 ${i <= step ? "bg-emerald-500 shadow-[0_0_8px_rgba(16,185,129,0.4)]" : "bg-zinc-800"}`} />
                            <span className={`text-[10px] font-medium block text-center transition-colors ${i <= step ? "text-emerald-400" : "text-zinc-600"}`}>
                                {s.icon} {s.title}
                            </span>
                        </div>
                    ))}
                </div>
            </CardHeader>
            <CardContent>
                <Form {...form}>
                    <form
                        onSubmit={form.handleSubmit(onSubmit)}
                        className="space-y-8"
                        onKeyDown={(e) => {
                            if (e.key === "Enter" && step < 3) {
                                e.preventDefault();
                            }
                        }}
                    >
                        {/* Animated step container */}
                        <div className="relative overflow-hidden min-h-[280px]"
                            onKeyDown={(e) => {
                                if (e.key === "Enter" && step < 3) {
                                    e.preventDefault();
                                    goNext();
                                }
                            }}>
                            <div key={step}
                                className={`animate-in duration-300 ${slideDir === "left" ? "slide-in-from-right-8" : "slide-in-from-left-8"} fade-in`}>
                                {step === 0 && <Step1 form={form} />}
                                {step === 1 && <Step2 form={form} />}
                                {step === 2 && <Step3 form={form} />}
                                {step === 3 && <Step4 form={form} />}
                            </div>
                        </div>

                        {/* Error */}
                        {error && (
                            <div className="flex items-start gap-3 rounded-lg border border-red-500/30 bg-red-500/10 p-4 text-red-400 text-sm animate-in fade-in duration-300">
                                <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5 shrink-0 mt-0.5" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"><circle cx="12" cy="12" r="10" /><line x1="12" y1="8" x2="12" y2="12" /><line x1="12" y1="16" x2="12.01" y2="16" /></svg>
                                <span><strong className="font-semibold text-red-300">Error: </strong>{error}</span>
                            </div>
                        )}

                        {/* Navigation */}
                        <div className="flex gap-3">
                            {step > 0 && (
                                <Button type="button" variant="outline" onClick={goBack}
                                    className="bg-transparent border-zinc-800 hover:bg-zinc-900 text-zinc-300 font-medium">
                                    <ChevronLeft className="w-4 h-4 mr-1" /> Back
                                </Button>
                            )}
                            <div className="flex-1" />
                            {step < 3 ? (
                                <Button type="button" onClick={goNext}
                                    className="bg-emerald-600 hover:bg-emerald-500 text-white font-medium px-8 h-11 transition-all shadow-[0_0_15px_rgba(16,185,129,0.2)] hover:shadow-[0_0_25px_rgba(16,185,129,0.4)]">
                                    Next <ChevronRight className="w-4 h-4 ml-1" />
                                </Button>
                            ) : (
                                <Button type="submit" disabled={isSubmitting || !submitReady}
                                    className={`bg-emerald-600 hover:bg-emerald-500 text-white font-medium px-8 h-12 transition-all shadow-[0_0_20px_rgba(16,185,129,0.2)] hover:shadow-[0_0_30px_rgba(16,185,129,0.4)] relative overflow-hidden group ${!submitReady ? 'opacity-50 cursor-not-allowed' : ''}`}>
                                    {isSubmitting ? (
                                        <div className="flex items-center gap-2">
                                            <div className="h-4 w-4 rounded-full border-2 border-white/20 border-t-white animate-spin" />
                                            Analyzing...
                                        </div>
                                    ) : "Generate Synthesis"}
                                    {submitReady && <div className="absolute inset-0 -translate-x-full group-hover:animate-[shimmer_1.5s_infinite] bg-gradient-to-r from-transparent via-white/10 to-transparent" />}
                                </Button>
                            )}
                        </div>
                    </form>
                </Form>
            </CardContent>
        </Card>
    );
}

// ═════════════════════════════════════════════════════════════════════════════
// WIZARD STEPS
// ═════════════════════════════════════════════════════════════════════════════
function Step1({ form }: { form: any }) {
    return (
        <div className="space-y-6">
            <h3 className="text-xl font-medium text-emerald-400 border-b border-white/10 pb-2 flex items-center gap-2">
                <span>{STEP_TITLES[0].icon}</span>{STEP_TITLES[0].num}. {STEP_TITLES[0].title}
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <FormField control={form.control} name="age" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300">Age</FormLabel><FormControl>
                        <Input type="number" placeholder="e.g. 25" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} value={field.value ?? ""} />
                    </FormControl><FormMessage className="text-red-400" /></FormItem>
                )} />
                <FormField control={form.control} name="gender" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300">Gender</FormLabel>
                        <Select onValueChange={field.onChange} defaultValue={field.value}>
                            <FormControl><SelectTrigger className="bg-zinc-900/50 border-zinc-800 text-white focus:ring-emerald-500/50"><SelectValue placeholder="Select gender" /></SelectTrigger></FormControl>
                            <SelectContent className="bg-zinc-950 border-zinc-800 text-zinc-200">
                                <SelectItem value="male">Male</SelectItem>
                                <SelectItem value="female">Female</SelectItem>
                            </SelectContent>
                        </Select><FormMessage className="text-red-400" /></FormItem>
                )} />
                <FormField control={form.control} name="height" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300">Height (cm)</FormLabel><FormControl>
                        <Input type="number" placeholder="e.g. 170" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} value={field.value ?? ""} />
                    </FormControl><FormMessage className="text-red-400" /></FormItem>
                )} />
                <FormField control={form.control} name="weight" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300">Weight (kg)</FormLabel><FormControl>
                        <Input type="number" placeholder="e.g. 70" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} value={field.value ?? ""} />
                    </FormControl><FormMessage className="text-red-400" /></FormItem>
                )} />
            </div>
        </div>
    );
}

function Step2({ form }: { form: any }) {
    return (
        <div className="space-y-6">
            <h3 className="text-xl font-medium text-emerald-400 border-b border-white/10 pb-2 flex items-center gap-2">
                <span>{STEP_TITLES[1].icon}</span> {STEP_TITLES[1].num}. {STEP_TITLES[1].title}
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <FormField control={form.control} name="activityLevel" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300 flex items-center gap-1.5"><Activity className="w-4 h-4 text-emerald-400" /> Activity Level</FormLabel>
                        <Select onValueChange={field.onChange} defaultValue={field.value}>
                            <FormControl><SelectTrigger className="bg-zinc-900/50 border-zinc-800 text-white focus:ring-emerald-500/50"><SelectValue placeholder="Select activity level" /></SelectTrigger></FormControl>
                            <SelectContent className="bg-zinc-950 border-zinc-800 text-zinc-200">
                                <SelectItem value="sedentary">Low (Sedentary)</SelectItem>
                                <SelectItem value="moderate">Medium</SelectItem>
                                <SelectItem value="active">High</SelectItem>
                            </SelectContent>
                        </Select><FormMessage className="text-red-400" /></FormItem>
                )} />
                <FormField control={form.control} name="dietType" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300">Diet Type</FormLabel>
                        <Select onValueChange={field.onChange} defaultValue={field.value}>
                            <FormControl><SelectTrigger className="bg-zinc-900/50 border-zinc-800 text-white focus:ring-emerald-500/50"><SelectValue placeholder="Select diet" /></SelectTrigger></FormControl>
                            <SelectContent className="bg-zinc-950 border-zinc-800 text-zinc-200">
                                <SelectItem value="balanced">Balanced</SelectItem>
                                <SelectItem value="high_sugar">High Sugar</SelectItem>
                                <SelectItem value="high_fat">High Fat</SelectItem>
                                <SelectItem value="vegetarian">Vegetarian / Vegan</SelectItem>
                                <SelectItem value="irregular">Irregular Meals</SelectItem>
                            </SelectContent>
                        </Select><FormMessage className="text-red-400" /></FormItem>
                )} />
                <FormField control={form.control} name="sleepHours" render={({ field }: any) => (
                    <FormItem><FormLabel className="text-zinc-300 flex items-center gap-1.5"><Moon className="w-4 h-4 text-emerald-400" /> Average Sleep (Hours)</FormLabel><FormControl>
                        <Input type="number" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} />
                    </FormControl><FormMessage className="text-red-400" /></FormItem>
                )} />
                <FormField control={form.control} name="stressLevel" render={({ field }: any) => (
                    <FormItem className="space-y-4">
                        <FormLabel className="text-zinc-300 flex items-center justify-between">
                            <span className="flex items-center gap-1.5"><Brain className="w-4 h-4 text-emerald-400" /> Stress Level</span>
                            <span className="text-sm font-medium text-emerald-400">{field.value} / 10</span>
                        </FormLabel><FormControl>
                            <Slider min={1} max={10} step={1} defaultValue={[field.value]} onValueChange={(vals: number[]) => field.onChange(vals[0])} className="py-1" />
                        </FormControl><FormMessage className="text-red-400" /></FormItem>
                )} />
            </div>
        </div>
    );
}

function Step3({ form }: { form: any }) {
    return (
        <div className="space-y-6">
            <div className="border-b border-white/10 pb-2">
                <h3 className="text-xl font-medium text-emerald-400 flex items-center gap-2">
                    <span>{STEP_TITLES[2].icon}</span> {STEP_TITLES[2].num}. {STEP_TITLES[2].title}
                </h3>
                <p className="text-sm text-zinc-500 pt-1">Select any current goals</p>
            </div>
            <FormField control={form.control} name="goals" render={() => (
                <FormItem>
                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
                        {HEALTH_GOALS.map((goal) => (
                            <FormField key={goal.id} control={form.control} name="goals" render={({ field }: any) => (
                                <FormItem className="flex flex-row items-start space-x-3 space-y-0 rounded-md border border-zinc-800 bg-zinc-900/30 p-4 hover:border-emerald-500/30 transition-colors cursor-pointer">
                                    <FormControl>
                                        <Checkbox checked={field.value?.includes(goal.id)}
                                            onCheckedChange={(checked: boolean) => checked ? field.onChange([...field.value, goal.id]) : field.onChange(field.value?.filter((v: string) => v !== goal.id))} />
                                    </FormControl>
                                    <FormLabel className="font-normal cursor-pointer text-zinc-300">{goal.label}</FormLabel>
                                </FormItem>
                            )} />
                        ))}
                    </div>
                    <FormMessage className="text-red-400 mt-2" />
                </FormItem>
            )} />
        </div>
    );
}

function Step4({ form }: { form: any }) {
    return (
        <div className="space-y-6">
            <div className="border-b border-white/10 pb-2">
                <h3 className="text-xl font-medium text-emerald-400 flex items-center gap-2">
                    <span>{STEP_TITLES[3].icon}</span> {STEP_TITLES[3].num}. {STEP_TITLES[3].title} (Optional)
                </h3>
                <p className="text-sm text-zinc-500 pt-1">Select any current symptoms.</p>
            </div>
            <FormField control={form.control} name="symptoms" render={() => (
                <FormItem>
                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
                        {SYMPTOMS.map((symptom) => (
                            <FormField key={symptom.id} control={form.control} name="symptoms" render={({ field }: any) => {
                                const arr = field.value || [];
                                return (
                                    <FormItem className="flex flex-row items-center space-x-3 space-y-0 rounded-md py-2">
                                        <FormControl>
                                            <Checkbox checked={arr.includes(symptom.id)}
                                                onCheckedChange={(checked: boolean) => checked ? field.onChange([...arr, symptom.id]) : field.onChange(arr.filter((v: string) => v !== symptom.id))} />
                                        </FormControl>
                                        <FormLabel className="font-normal cursor-pointer text-zinc-400">{symptom.label}</FormLabel>
                                    </FormItem>
                                );
                            }} />
                        ))}
                    </div>
                </FormItem>
            )} />
        </div>
    );
}

// END OF WIZARD STEPS
