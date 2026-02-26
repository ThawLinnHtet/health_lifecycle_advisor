import { NextResponse } from "next/server";

export const dynamic = 'force-dynamic';
export const fetchCache = 'force-no-store';
export async function POST(req: Request) {
    try {
        const body = await req.json();

        const prologServerUrl = process.env.PROLOG_SERVER_URL || "http://127.0.0.1:8000/advice";

        try {
            const response = await fetch(prologServerUrl, {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify(body),
            });

            if (!response.ok) {
                const errorText = await response.text();
                throw new Error(`Prolog server error ${response.status}: ${errorText}`);
            }

            // Force UTF-8 decoding before parsing JSON
            const arrayBuffer = await response.arrayBuffer();
            const decoder = new TextDecoder('utf-8');
            const jsonString = decoder.decode(arrayBuffer);
            const adviceResult = JSON.parse(jsonString);

            return NextResponse.json(adviceResult);
        } catch (networkError) {
            console.error("Failed to connect to Prolog server:", networkError);
            return NextResponse.json(
                { error: "Prolog Engine Unreachable. Ensure SWI-Prolog is running." },
                { status: 503 }
            );
        }

    } catch (error) {
        console.error("Error in /api/advice route:", error);
        return NextResponse.json(
            { error: "Internal Server Error" },
            { status: 500 }
        );
    }
}
