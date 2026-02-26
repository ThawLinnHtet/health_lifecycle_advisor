"use client";

import { useState } from "react";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import * as z from "zod";
import { Activity, HeartPulse, Moon, Brain, ChevronRight } from "lucide-react";

import { Button } from "@/components/ui/button";
import {
    Form,
    FormControl,
    FormDescription,
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

import { RadioGroup, RadioGroupItem } from "@/components/ui/radio-group";
import { Checkbox } from "@/components/ui/checkbox";

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

const formSchema = z.object({
    // Section 1: Basic Info
    age: z.coerce.number().min(0).max(120),
    gender: z.enum(["male", "female", "other"], {
        message: "Please select a gender."
    }),
    height: z.coerce.number().min(50).max(300, "Enter height in cm"),
    weight: z.coerce.number().min(20).max(500, "Enter weight in kg"),

    // Section 2: Lifestyle
    activityLevel: z.string().min(1, "Please select an activity level."),
    dietType: z.string().min(1, "Please select a diet type."),
    sleepHours: z.coerce.number().min(0).max(24),
    stressLevel: z.coerce.number().min(1).max(10).default(5), // from Slider

    // Section 3: Goals
    goals: z.array(z.string()).min(1, "Select at least one goal."),

    // Section 4: Symptoms (Optional)
    symptoms: z.array(z.string()).optional(),
});

type FormValues = z.infer<typeof formSchema>;

export function AdvisorForm() {
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [result, setResult] = useState<any>(null);

    const form = useForm<FormValues>({
        resolver: zodResolver(formSchema) as any,
        defaultValues: {
            age: 25,
            gender: undefined, // undefined to trigger required error if empty
            height: 170,
            weight: 70,
            activityLevel: "",
            dietType: "",
            sleepHours: 7,
            stressLevel: 5,
            goals: [],
            symptoms: [],
        },
    });

    async function onSubmit(values: FormValues) {
        setIsSubmitting(true);
        try {
            const payload = {
                ...values,
                // Convert the string array to a comma separated string so Prolog can parse easier, or send as array
                goals: values.goals,
                symptoms: values.symptoms
            };

            const response = await fetch('/api/advice', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(payload)
            });

            if (!response.ok) throw new Error("API Route failed");
            const adviceResult = await response.json();
            setResult(adviceResult);
        } catch (error) {
            console.error(error);
        } finally {
            setIsSubmitting(false);
        }
    }

    if (result) {
        return (
            <Card className="w-full max-w-2xl mx-auto border-emerald-500/20 bg-black/40 backdrop-blur-xl animate-in fade-in zoom-in-95 duration-500">
                <CardHeader className="space-y-1 pb-4 border-b border-white/5">
                    <CardTitle className="text-2xl font-semibold flex items-center gap-2 text-emerald-400">
                        <HeartPulse className="h-6 w-6" />
                        Your Health Action Plan
                    </CardTitle>
                    <CardDescription className="text-zinc-400 text-base flex flex-col pt-4 gap-2">
                        <span className="flex items-center gap-2">
                            <span className="text-zinc-500 font-medium w-32">Life Stage:</span>
                            <strong className="text-emerald-300 bg-emerald-500/10 px-2 py-0.5 rounded-md">{result.stage}</strong>
                        </span>
                        <span className="flex items-start gap-2">
                            <span className="text-zinc-500 font-medium w-32 shrink-0">Priority Area:</span>
                            <strong className="text-zinc-200 mt-0.5">{result.focusArea}</strong>
                        </span>
                        {result.bmi > 0 && (
                            <span className="flex items-center gap-2 pt-1 border-t border-white/5 mt-1">
                                <span className="text-zinc-500 font-medium w-32">Current BMI:</span>
                                <strong className="text-white">{result.bmi.toFixed(1)}</strong>
                                <span className="text-zinc-400 text-sm">({result.bmiCategory})</span>
                            </span>
                        )}
                    </CardDescription>
                </CardHeader>
                <CardContent className="pt-6 space-y-6">
                    <div>
                        <h4 className="text-emerald-400 font-medium mb-4 text-lg">Recommended Actions:</h4>
                        <ul className="space-y-4 bg-zinc-900/40 p-5 rounded-lg border border-white/5">
                            {result.advice.map((item: string, i: number) => (
                                <li key={i} className="flex gap-3 text-zinc-300 items-start">
                                    <ChevronRight className="h-5 w-5 text-emerald-500 shrink-0 mt-0.5" />
                                    <span className="leading-relaxed">{item}</span>
                                </li>
                            ))}
                        </ul>
                    </div>
                    <div className="pt-4">
                        <Button
                            variant="outline"
                            className="w-full bg-transparent border-zinc-800 hover:bg-zinc-900 text-zinc-300 transition-all font-medium"
                            onClick={() => setResult(null)}
                        >
                            Analyze New Profile
                        </Button>
                    </div>
                </CardContent>
            </Card>
        );
    }

    return (
        <Card className="w-full max-w-2xl mx-auto border-white/10 bg-black/40 backdrop-blur-xl shadow-2xl">
            <CardHeader className="space-y-2 pb-8">
                <CardTitle className="text-3xl font-semibold tracking-tight text-white">
                    Lifestyle Profile
                </CardTitle>
                <CardDescription className="text-zinc-400 text-base">
                    Complete the 4 sections below to receive highly personalized, AI-driven health optimizations based on your unique profile and goals.
                </CardDescription>
            </CardHeader>
            <CardContent>
                <Form {...form}>
                    <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-12">

                        {/* SECTION 1: BASIC INFO */}
                        <div className="space-y-6">
                            <div className="border-b border-white/10 pb-2">
                                <h3 className="text-xl font-medium text-emerald-400">1. Basic Information</h3>
                            </div>
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                                <FormField control={form.control} name="age" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300">Age</FormLabel>
                                        <FormControl>
                                            <Input type="number" placeholder="25" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} />
                                        </FormControl>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                                <FormField control={form.control} name="gender" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300">Gender</FormLabel>
                                        <Select onValueChange={field.onChange} defaultValue={field.value}>
                                            <FormControl>
                                                <SelectTrigger className="bg-zinc-900/50 border-zinc-800 text-white focus:ring-emerald-500/50">
                                                    <SelectValue placeholder="Select gender" />
                                                </SelectTrigger>
                                            </FormControl>
                                            <SelectContent className="bg-zinc-950 border-zinc-800 text-zinc-200">
                                                <SelectItem value="male">Male</SelectItem>
                                                <SelectItem value="female">Female</SelectItem>
                                                <SelectItem value="other">Other / Prefer not to say</SelectItem>
                                            </SelectContent>
                                        </Select>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                                <FormField control={form.control} name="height" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300">Height (cm)</FormLabel>
                                        <FormControl>
                                            <Input type="number" placeholder="170" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} />
                                        </FormControl>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                                <FormField control={form.control} name="weight" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300">Weight (kg)</FormLabel>
                                        <FormControl>
                                            <Input type="number" placeholder="70" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} />
                                        </FormControl>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                            </div>
                        </div>

                        {/* SECTION 2: LIFESTYLE */}
                        <div className="space-y-6">
                            <div className="border-b border-white/10 pb-2">
                                <h3 className="text-xl font-medium text-emerald-400">2. Lifestyle Factors</h3>
                            </div>
                            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                                <FormField control={form.control} name="activityLevel" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300 flex items-center gap-1.5"><Activity className="w-4 h-4 text-emerald-400" /> Activity Level</FormLabel>
                                        <Select onValueChange={field.onChange} defaultValue={field.value}>
                                            <FormControl>
                                                <SelectTrigger className="bg-zinc-900/50 border-zinc-800 text-white focus:ring-emerald-500/50">
                                                    <SelectValue placeholder="Select activity level" />
                                                </SelectTrigger>
                                            </FormControl>
                                            <SelectContent className="bg-zinc-950 border-zinc-800 text-zinc-200">
                                                <SelectItem value="sedentary">Low (Sedentary)</SelectItem>
                                                <SelectItem value="moderate">Medium</SelectItem>
                                                <SelectItem value="active">High</SelectItem>
                                            </SelectContent>
                                        </Select>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                                <FormField control={form.control} name="dietType" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300">Diet Type</FormLabel>
                                        <Select onValueChange={field.onChange} defaultValue={field.value}>
                                            <FormControl>
                                                <SelectTrigger className="bg-zinc-900/50 border-zinc-800 text-white focus:ring-emerald-500/50">
                                                    <SelectValue placeholder="Select diet" />
                                                </SelectTrigger>
                                            </FormControl>
                                            <SelectContent className="bg-zinc-950 border-zinc-800 text-zinc-200">
                                                <SelectItem value="balanced">Balanced</SelectItem>
                                                <SelectItem value="high_sugar">High Sugar</SelectItem>
                                                <SelectItem value="high_fat">High Fat</SelectItem>
                                                <SelectItem value="vegetarian">Vegetarian / Vegan</SelectItem>
                                                <SelectItem value="irregular">Irregular Meals</SelectItem>
                                            </SelectContent>
                                        </Select>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                                <FormField control={form.control} name="sleepHours" render={({ field }) => (
                                    <FormItem>
                                        <FormLabel className="text-zinc-300 flex items-center gap-1.5"><Moon className="w-4 h-4 text-emerald-400" /> Average Sleep (Hours)</FormLabel>
                                        <FormControl>
                                            <Input type="number" className="bg-zinc-900/50 border-zinc-800 text-white focus-visible:ring-emerald-500/50" {...field} />
                                        </FormControl>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                                <FormField control={form.control} name="stressLevel" render={({ field }) => (
                                    <FormItem className="space-y-4">
                                        <FormLabel className="text-zinc-300 flex items-center justify-between">
                                            <span className="flex items-center gap-1.5"><Brain className="w-4 h-4 text-emerald-400" /> Stress Level</span>
                                            <span className="text-sm font-medium text-emerald-400">{field.value} / 10</span>
                                        </FormLabel>
                                        <FormControl>
                                            <Slider min={1} max={10} step={1} defaultValue={[field.value]} onValueChange={(vals) => field.onChange(vals[0])} className="py-1" />
                                        </FormControl>
                                        <FormMessage className="text-red-400" />
                                    </FormItem>
                                )}
                                />
                            </div>
                        </div>

                        {/* SECTION 3: HEALTH GOALS */}
                        <div className="space-y-6">
                            <div className="border-b border-white/10 pb-2">
                                <h3 className="text-xl font-medium text-emerald-400">3. Health Goals</h3>
                                <p className="text-sm text-zinc-500 pt-1">Select all that apply.</p>
                            </div>
                            <FormField control={form.control} name="goals" render={() => (
                                <FormItem>
                                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
                                        {HEALTH_GOALS.map((goal) => (
                                            <FormField key={goal.id} control={form.control} name="goals" render={({ field }) => {
                                                return (
                                                    <FormItem key={goal.id} className="flex flex-row items-start space-x-3 space-y-0 rounded-md border border-zinc-800 bg-zinc-900/30 p-4">
                                                        <FormControl>
                                                            <Checkbox
                                                                checked={field.value?.includes(goal.id)}
                                                                onCheckedChange={(checked) => {
                                                                    return checked
                                                                        ? field.onChange([...field.value, goal.id])
                                                                        : field.onChange(field.value?.filter((value) => value !== goal.id))
                                                                }}
                                                            />
                                                        </FormControl>
                                                        <FormLabel className="font-normal cursor-pointer text-zinc-300">
                                                            {goal.label}
                                                        </FormLabel>
                                                    </FormItem>
                                                )
                                            }} />
                                        ))}
                                    </div>
                                    <FormMessage className="text-red-400 mt-2" />
                                </FormItem>
                            )} />
                        </div>

                        {/* SECTION 4: SYMPTOMS */}
                        <div className="space-y-6">
                            <div className="border-b border-white/10 pb-2">
                                <h3 className="text-xl font-medium text-emerald-400">4. Symptoms (Optional)</h3>
                                <p className="text-sm text-zinc-500 pt-1">Select any current symptoms you are experiencing.</p>
                            </div>
                            <FormField control={form.control} name="symptoms" render={() => (
                                <FormItem>
                                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
                                        {SYMPTOMS.map((symptom) => (
                                            <FormField key={symptom.id} control={form.control} name="symptoms" render={({ field }) => {
                                                const fallbackArr = field.value || [];
                                                return (
                                                    <FormItem key={symptom.id} className="flex flex-row items-center space-x-3 space-y-0 rounded-md py-2">
                                                        <FormControl>
                                                            <Checkbox
                                                                checked={fallbackArr.includes(symptom.id)}
                                                                onCheckedChange={(checked) => {
                                                                    return checked
                                                                        ? field.onChange([...fallbackArr, symptom.id])
                                                                        : field.onChange(fallbackArr.filter((value) => value !== symptom.id))
                                                                }}
                                                            />
                                                        </FormControl>
                                                        <FormLabel className="font-normal cursor-pointer text-zinc-400">
                                                            {symptom.label}
                                                        </FormLabel>
                                                    </FormItem>
                                                )
                                            }} />
                                        ))}
                                    </div>
                                </FormItem>
                            )} />
                        </div>

                        <Button
                            type="submit"
                            disabled={isSubmitting}
                            className="w-full bg-emerald-600 hover:bg-emerald-500 text-white font-medium h-12 transition-all shadow-[0_0_20px_rgba(16,185,129,0.2)] hover:shadow-[0_0_30px_rgba(16,185,129,0.4)] relative overflow-hidden group"
                        >
                            {isSubmitting ? (
                                <div className="flex items-center gap-2">
                                    <div className="h-4 w-4 rounded-full border-2 border-white/20 border-t-white animate-spin" />
                                    Analyzing Profile...
                                </div>
                            ) : (
                                "Generate Synthesis"
                            )}
                            <div className="absolute inset-0 -translate-x-full group-hover:animate-[shimmer_1.5s_infinite] bg-gradient-to-r from-transparent via-white/10 to-transparent" />
                        </Button>
                    </form>
                </Form>
            </CardContent>
        </Card>
    );
}
