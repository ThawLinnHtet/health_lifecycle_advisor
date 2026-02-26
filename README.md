# Aura | Health & Lifecycle Advisor

Aura is an intelligent health insights platform that combines a modern **Next.js** frontend with a powerful **SWI-Prolog** backend reasoning engine. It takes user lifestyle profiles (age, activity level, diet, sleep, etc.) and uses Prolog's logical rule evaluation to generate a highly personalized health action plan.

## Features

- **Modern UI**: Built with Next.js 14+, Tailwind CSS, and Shadcn UI components for a stunning, glassmorphism-inspired dark mode interface.
- **Prolog Inference Engine**: A dedicated SWI-Prolog HTTP server acts as the backend logic layer, evaluating conditional rules intersecting goals, diets, and symptoms.
- **Comprehensive Analysis**: Dynamically determines lifecycle stages, calculates BMI categories, and spots symptom patterns like burnout or chronic fatigue.

## Architecture

1. **Frontend**: Next.js React application (`/app` and `/components`).
2. **API Route**: Next.js serverless route (`/api/advice/route.ts`) serving as a bridge to avoid CORS issues.
3. **Prolog Server**: SWI-Prolog running a local HTTP server (`/prolog/server.pl` and `/prolog/knowledge_base.pl`).

## Prerequisites

To run this project locally, you must have both Node.js and SWI-Prolog installed on your machine.

1. **Node.js**: [Download Here](https://nodejs.org/)
2. **SWI-Prolog**: [Download Here](https://www.swi-prolog.org/download/stable)

> **Important (Windows Users):** Ensure that the `swipl` executable is in your system's `PATH` environment variable. Typically, you need to add `C:\Program Files\swipl\bin` to your System Path so the Next.js start script can launch the Prolog server automatically.

## Getting Started

1. Clone the repository and install dependencies:
   ```bash
   npm install
   ```

2. Start the development servers:
   ```bash
   npm run dev:all
   ```
   *Note: This command attempts to start both Next.js and the SWI-Prolog engine concurrently. If Prolog fails to start due to PATH issues, you can start them separately.*

   **Starting Manually (Alternative):**
   Terminal 1 (Next.js):
   ```bash
   npm run dev
   ```
   Terminal 2 (Prolog API):
   ```bash
   swipl -s prolog/server.pl -g "server(8000), thread_get_message(_)."
   ```

3. Open [http://localhost:3000](http://localhost:3000) in your browser to view the application.

## Deployment

**Frontend (Vercel):** The Next.js repository can be connected directly to Vercel for instant deployment. 
**Backend (Prolog):** SWI-Prolog requires a persistent host (e.g., Render, Railway, DigitalOcean, AWS EC2) or a Dockerized container to run. Ensure you update the `PROLOG_SERVER_URL` environment variable in Vercel to point to your hosted Prolog instance.
