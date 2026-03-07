:- encoding(utf8).
:- module(knowledge_base, [
    analyze_profile/2
]).

%======================================================================
% Core API: analyze_profile(+ProfileDict, -ResultDict)
%======================================================================
analyze_profile(Profile, Result) :-
    % 1. Extract Core Data
    Age = Profile.get(age),
    Weight = Profile.get(weight),
    Height = Profile.get(height),
    
    % 2. Determine Lifecycle Stage
    lifecycle_stage(Age, Stage),
    focus_area(Age, Stage, FocusArea),

    % 3. Calculate BMI & Risk Category
    (Height > 0 -> BMI is Weight / ((Height / 100) * (Height / 100)) ; BMI is 0),
    bmi_category(BMI, BMICategory),

    % 4. Calculate composite health score (0-100)
    health_score(Profile, BMI, BMICategory, Score),
    score_grade(Score, Grade),

    % 5. Generate Advice (Collect all matching rules)
    findall(AdvicePart, health_advice(Profile, Stage, BMI, BMICategory, AdvicePart), AdviceList),
    
    % Remove duplicates if any rules overlapped
    sort(AdviceList, UniqueAdviceList),

    % Ensure there is at least one piece of advice! (Fallback)
    (   UniqueAdviceList == []
    ->  FinalAdviceList = ['Your profile looks incredibly balanced right now! Keep up your current routines, focusing on consistency and enjoying the process.']
    ;   FinalAdviceList = UniqueAdviceList
    ),

    % 6. Package everything up into a Dict
    Result = _{
        stage: Stage,
        focusArea: FocusArea,
        bmi: BMI,
        bmiCategory: BMICategory,
        score: Score,
        grade: Grade,
        advice: FinalAdviceList
    }.

%======================================================================
% 1. Lifecycle Stage Rules
%======================================================================
lifecycle_stage(Age, 'Infant')         :- Age >= 0, Age < 3, !.
lifecycle_stage(Age, 'Child')          :- Age >= 3, Age < 13, !.
lifecycle_stage(Age, 'Teenager')       :- Age >= 13, Age < 20, !.
lifecycle_stage(Age, 'Young Adult')    :- Age >= 20, Age < 35, !.
lifecycle_stage(Age, 'Adult')          :- Age >= 35, Age < 60, !.
lifecycle_stage(Age, 'Senior')         :- Age >= 60, !.
lifecycle_stage(_, 'Unknown Stage').   % Catch-all for invalid ages (e.g., negative)

%======================================================================
% 2. Focus Area Rules (Tied to Lifecycle)
%======================================================================
focus_area(_, 'Infant',       'Early Development, Motor Skills, & Neurological Growth') :- !.
focus_area(_, 'Child',        'Cognitive Development & Fundamental Nutrition') :- !.
focus_area(_, 'Teenager',     'Hormonal Balance, Mental Wellbeing, & Bone Density') :- !.
focus_area(_, 'Young Adult',  'Fitness Foundation, Metabolic Health, & Stress Resilience') :- !.
focus_area(_, 'Adult',        'Cardiovascular Maintenance, Joint Health, & Disease Prevention') :- !.
focus_area(_, 'Senior',       'Mobility Preservation, Cognitive Sharpness, & Longevity') :- !.
% FIX: Fallback so 'Unknown Stage' does not crash analyze_profile/2
focus_area(_, 'Unknown Stage', 'General Health & Wellbeing').

%======================================================================
% 3. BMI Categorization Rules
%======================================================================
bmi_category(BMI, 'Underweight')   :- BMI > 0, BMI < 18.5, !.
bmi_category(BMI, 'Normal weight') :- BMI >= 18.5, BMI < 25.0, !.
bmi_category(BMI, 'Overweight')    :- BMI >= 25.0, BMI < 30.0, !.
bmi_category(BMI, 'Obese')         :- BMI >= 30.0, !.
bmi_category(_, 'Unknown').

%======================================================================
% 4. Health Advice Rules
% Rules map the complex profile data to specific recommendations.
%======================================================================

% -----------------------------------------------------------------------
% --- BMI Advice ---
% -----------------------------------------------------------------------
health_advice(_, _, _, 'Underweight', 'Your BMI indicates you might be a bit underweight. Consider speaking with a nutritionist or dietitian—they can help you safely add delicious, nutrient-dense calories to your daily meals!').
health_advice(_, _, _, 'Overweight', 'Your BMI is currently in the overweight category. Don\'t stress! Focusing on a slight caloric deficit and simply increasing your daily step count can work wonders for your long-term health.').
health_advice(_, _, _, 'Obese', 'Your BMI indicates obesity, which can put extra strain on your body. The best first step is to partner with a healthcare professional to create a sustainable, low-impact plan that you actually enjoy.').

% -----------------------------------------------------------------------
% --- Sleep Advice ---
% -----------------------------------------------------------------------
health_advice(Profile, _, _, _, 'We noticed you are getting less than 7 hours of sleep. Try prioritizing 7-8 hours a night—it is the ultimate cheat code for recovery, focus, and hormone balance!') :-
    Sleep = Profile.get(sleepHours),
    Sleep < 7.

% -----------------------------------------------------------------------
% --- Activity Advice ---
% -----------------------------------------------------------------------
% FIX: Include 'light' alongside 'sedentary' to match all low-activity cases
health_advice(Profile, _, _, _, 'A sedentary or low-activity lifestyle can sneak up on you! Try to gradually build up to 30 minutes of light movement today, even if it is just a brisk walk while listening to a podcast.') :-
    Activity = Profile.get(activityLevel),
    (Activity = 'sedentary' ; Activity = 'light').

% -----------------------------------------------------------------------
% --- Diet Advice ---
% -----------------------------------------------------------------------
% Diet + Goal intersections (existing, preserved)
health_advice(Profile, _, _, _, 'Since you want to gain muscle on a vegetarian diet, protein is your best friend! Make sure you are loading up on plant-based powerhouses like quinoa, lentils, soy, and hemp seeds.') :-
    Goals = Profile.get(goals),
    member('muscle_gain', Goals),
    Diet = Profile.get(dietType),
    Diet = 'vegetarian'.

health_advice(Profile, _, _, _, 'A high-sugar diet makes weight loss incredibly difficult because it constantly spikes your insulin. Try swapping sugary snacks for whole fruits to crush those cravings naturally!') :-
    Goals = Profile.get(goals),
    member('weight_loss', Goals),
    Diet = Profile.get(dietType),
    Diet = 'high_sugar'.

health_advice(Profile, _, _, _, 'Since heart health is a major goal for you, let\'s watch that high-fat diet. Try shifting towards "healthy fats" like avocados, nuts, and olive oil to protect your cardiovascular system.') :-
    Goals = Profile.get(goals),
    member('heart_health', Goals),
    Diet = Profile.get(dietType),
    Diet = 'high_fat'.

% FIX: Stand-alone advice for 'balanced' diet (previously no rule existed)
health_advice(Profile, _, _, _, 'Great job maintaining a balanced diet—this is one of the most powerful things you can do for your long-term health! Keep focusing on whole foods, colourful vegetables, and lean proteins.') :-
    Diet = Profile.get(dietType),
    Diet = 'balanced'.

% FIX: Stand-alone advice for 'irregular' diet (previously no rule existed)
health_advice(Profile, _, _, _, 'Irregular meal patterns can disrupt your metabolism and blood sugar, leading to energy crashes and cravings. Try meal prepping a few simple, consistent meals each week to build a more stable eating rhythm.') :-
    Diet = Profile.get(dietType),
    Diet = 'irregular'.

% FIX: High-sugar diet advice even without a specific weight-loss goal
health_advice(Profile, _, _, _, 'High sugar intake is strongly linked to inflammation, energy crashes, and long-term metabolic damage. Try replacing processed sweets with whole fruit, and read labels to spot hidden sugars in everyday foods.') :-
    Diet = Profile.get(dietType),
    Diet = 'high_sugar',
    Goals = Profile.get(goals),
    \+ member('weight_loss', Goals).

% FIX: High-fat diet advice even without heart_health goal
health_advice(Profile, _, _, _, 'A high-fat diet can be healthy if the fats are the right kind! Prioritise unsaturated fats from sources like nuts, seeds, and oily fish. Minimise saturated and trans fats found in processed foods.') :-
    Diet = Profile.get(dietType),
    Diet = 'high_fat',
    Goals = Profile.get(goals),
    \+ member('heart_health', Goals).

% -----------------------------------------------------------------------
% --- Symptom Pattern Matching Rules ---
% -----------------------------------------------------------------------
health_advice(Profile, _, _, _, '**BURNOUT WARNING**: High stress, constant fatigue, and brain fog are your body\'s emergency signals. Please prioritize active recovery today, take mental breaks, and don\'t hesitate to seek professional support. You cannot pour from an empty cup!') :-
    Stress = Profile.get(stressLevel),
    Stress > 7,
    Symptoms = Profile.get(symptoms),
    member('fatigue', Symptoms),
    member('poor_concentration', Symptoms).

health_advice(Profile, _, _, _, 'Chronic back pain mixed with a lot of sitting usually points to a weak core. A super basic 5-minute daily core-strengthening routine could change your life!') :-
    Activity = Profile.get(activityLevel),
    (Activity = 'sedentary' ; Activity = 'light'),
    Symptoms = Profile.get(symptoms),
    member('back_pain', Symptoms).

health_advice(Profile, _, _, _, 'Frequent headaches can be awful! If you look at screens all day, remember the 20-20-20 rule to rest your eyes, and keep a water bottle glued to your side.') :-
    Symptoms = Profile.get(symptoms),
    member('headaches', Symptoms).

health_advice(Profile, _, _, _, 'Shortness of breath during daily activities can be an early sign of cardiovascular deconditioning. Start with gentle aerobic exercises like walking or swimming, and consult a doctor if it persists or worsens.') :-
    Symptoms = Profile.get(symptoms),
    member('shortness_breath', Symptoms).

health_advice(Profile, _, _, _, 'Persistent fatigue is your body asking for more support. Prioritise consistent sleep, check your iron and Vitamin D levels with a simple blood test, and ensure you are eating enough calories for your activity level.') :-
    Symptoms = Profile.get(symptoms),
    member('fatigue', Symptoms),
    Stress = Profile.get(stressLevel),
    Stress =< 7. % Avoid duplicate with burnout rule above

health_advice(Profile, _, _, _, 'Poor concentration can stem from inadequate sleep, high stress, or nutritional gaps. Try the Pomodoro technique (25-min focused blocks) and ensure you are getting enough Omega-3 fatty acids like those found in walnuts and fish.') :-
    Symptoms = Profile.get(symptoms),
    member('poor_concentration', Symptoms),
    Stress = Profile.get(stressLevel),
    Stress =< 7. % Avoid duplicate with burnout rule above

% -----------------------------------------------------------------------
% --- Goal Personalization ---
% -----------------------------------------------------------------------
health_advice(Profile, _, _, _, 'To hit your goal of reducing stress, try adding just 10 minutes of mindfulness or deep breathing to your morning. It genuinely lowers cortisol levels.') :-
    Goals = Profile.get(goals),
    member('reduce_stress', Goals).

health_advice(Profile, _, _, _, 'Improving sleep is a fantastic goal! Try establishing a strict "wind-down" routine: absolutely no screens 1 hour before bed, and keep your room cool and dark.') :-
    Goals = Profile.get(goals),
    member('improve_sleep', Goals).

% FIX: Add rule for 'general_wellness' goal (previously had no rule)
health_advice(Profile, _, _, _, 'For general wellness, consistency beats intensity every time. Focus on three pillars daily: at least 7 hours of sleep, 30 minutes of movement, and one genuinely nutritious meal.') :-
    Goals = Profile.get(goals),
    member('general_wellness', Goals).

% -----------------------------------------------------------------------
% --- Gender-Aware Advice (NEW — previously gender was collected but ignored)
% -----------------------------------------------------------------------
health_advice(Profile, _, _, _, 'As a woman, iron and folate are especially important nutrients to monitor. Include leafy greens, legumes, and fortified foods in your diet, and consider a regular blood panel to check your levels.') :-
    Gender = Profile.get(gender),
    Gender = 'female'.

health_advice(Profile, _, _, _, 'Men are statistically more likely to underestimate cardiovascular risk. Even at a young age, regular blood pressure checks and keeping an eye on your LDL cholesterol level are smart habits to build now.') :-
    Gender = Profile.get(gender),
    Gender = 'male'.

% -----------------------------------------------------------------------
% --- Stage-Specific Baseline Advice ---
% -----------------------------------------------------------------------

% --- Infant (NEW — previously no advice existed for this stage) ---
health_advice(_, 'Infant', _, _, 'During the infant stage, the most critical factor is proper nutrition through breast milk or iron-fortified formula, alongside regular paediatric check-ups to monitor developmental milestones. Supervised tummy time is great for motor development!').
health_advice(_, 'Infant', _, _, 'Sleep is the primary driver of infant brain development. Aim for a safe sleep environment: always place infants on their back on a firm, flat surface with no loose bedding.').

% --- Child (NEW — previously no advice existed for this stage) ---
health_advice(_, 'Child', _, _, 'This is a critical window for building healthy habits that last a lifetime. Encourage a variety of colourful whole foods, limit highly processed snacks, and make sure physical play is a daily part of their routine.').
health_advice(_, 'Child', _, _, 'Children aged 6-12 need at least 60 minutes of moderate-to-vigorous physical activity daily. Activities like swimming, cycling, or team sports also build crucial social and coordination skills.').
health_advice(Profile, 'Child', _, _, 'Screen time should be balanced with physical and creative activities. For school-age children, aim for no more than 2 hours of recreational screen time per day and ensure 9-11 hours of sleep nightly.') :-
    Sleep = Profile.get(sleepHours),
    Sleep < 9.

% --- Teenager (NEW — previously no advice existed for this stage) ---
health_advice(_, 'Teenager', _, _, 'Adolescence brings massive hormonal changes. Prioritise calcium-rich foods (dairy, leafy greens, fortified alternatives) and Vitamin D now — this is your peak window for building lifelong bone density.').
health_advice(Profile, 'Teenager', _, _, 'As a teenager, your mental health matters just as much as your physical health. High stress and mood swings are common, but talking to someone you trust or a counsellor can make a huge difference.') :-
    Stress = Profile.get(stressLevel),
    Stress >= 6.
health_advice(Profile, 'Teenager', _, _, 'Teenagers need 8-10 hours of sleep per night for healthy brain development. Poor sleep during adolescence is directly linked to worse academic performance and mood regulation.') :-
    Sleep = Profile.get(sleepHours),
    Sleep < 8.
health_advice(_, 'Teenager', _, _, 'Building a consistent exercise habit in your teens pays dividends for decades. Aim for at least 60 minutes of activity daily, combining aerobic exercise with muscle-strengthening activities at least 3 days a week.').

% --- Young Adult (Depends on Activity) ---
health_advice(Profile, 'Young Adult', _, _, 'As a Young Adult, you are in the prime window to build peak bone mass and heart capacity. Since you are already active, keep safely pushing your limits to build an unbreakable foundation for later years!') :-
    Activity = Profile.get(activityLevel),
    (Activity = 'moderate' ; Activity = 'active').

health_advice(Profile, 'Young Adult', _, _, 'You are in the prime window to build peak bone mass, but your low activity puts you at risk for future issues. Start some light weight-bearing exercises this week—your future self will thank you!') :-
    Activity = Profile.get(activityLevel),
    (Activity = 'sedentary' ; Activity = 'light').

% --- Adult (Depends on Stress) ---
health_advice(Profile, 'Adult', _, _, 'Metabolism naturally slows down. Since your stress is managed well, focus on nutrient-dense foods to maintain your healthy baseline.') :-
    Stress = Profile.get(stressLevel),
    Stress < 6.

health_advice(Profile, 'Adult', _, _, 'Metabolism naturally slows down, and your high stress levels will aggressively increase fat storage around your organs. Managing stress is your #1 priority right now.') :-
    Stress = Profile.get(stressLevel),
    Stress >= 6.

% --- Senior (Depends on Sleep) ---
health_advice(Profile, 'Senior', _, _, 'Focus on resistance training to combat sarcopenia (muscle loss). Your good sleep habits will perfectly support this muscle recovery.') :-
    Sleep = Profile.get(sleepHours),
    Sleep >= 7.

health_advice(Profile, 'Senior', _, _, 'To prevent cognitive decline and muscle loss, you MUST improve your sleep duration. Deep sleep is when the brain clears toxins and consolidates memories.') :-
    Sleep = Profile.get(sleepHours),
    Sleep < 7.

health_advice(_, 'Senior', _, _, 'Balance and fall-prevention exercises like Tai Chi or yoga are especially important for seniors. Falls are the leading cause of injury in older adults and are largely preventable with regular practice.').

%======================================================================
% 5. Health Score Calculation (0-100 composite)
%   Weights: BMI=30, Sleep=25, Stress=25, Activity=20
%======================================================================
health_score(Profile, BMI, BMICategory, FinalScore) :-
    % BMI component (0-30 points)
    bmi_points(BMI, BMICategory, BMIPoints),
    % Sleep component (0-25 points)
    Sleep = Profile.get(sleepHours),
    sleep_points(Sleep, SleepPoints),
    % Stress component (0-25 points)
    Stress = Profile.get(stressLevel),
    stress_points(Stress, StressPoints),
    % Activity component (0-20 points)
    Activity = Profile.get(activityLevel),
    activity_points(Activity, ActivityPoints),
    RawScore is BMIPoints + SleepPoints + StressPoints + ActivityPoints,
    FinalScore is max(0, min(100, round(RawScore))).

% BMI scoring (max 30)
bmi_points(_, 'Normal weight', 30) :- !.
bmi_points(BMI, 'Underweight', Points) :- Points is max(0, 30 - abs(18.5 - BMI) * 4), !.
bmi_points(BMI, 'Overweight', Points) :- Points is max(0, 30 - (BMI - 25) * 4), !.
bmi_points(BMI, 'Obese', Points) :- Points is max(0, 30 - (BMI - 25) * 3), !.
bmi_points(_, _, 15). % Unknown

% Sleep scoring (max 25) — optimal 7-9h
sleep_points(S, 25) :- S >= 7, S =< 9, !.
sleep_points(S, Points) :- S < 7, Points is max(0, 25 - (7 - S) * 6), !.
sleep_points(S, Points) :- S > 9, Points is max(0, 25 - (S - 9) * 4), !.
sleep_points(_, 10).

% Stress scoring (max 25) — lower is better
stress_points(Stress, Points) :-
    Points is max(0, round(25 * (1 - (Stress - 1) / 9))).

% Activity scoring (max 20)
activity_points('active', 20) :- !.
activity_points('moderate', 15) :- !.
activity_points('light', 8) :- !.
activity_points('sedentary', 3) :- !.
activity_points(_, 10).

%======================================================================
% 6. Score Grade Mapping
%======================================================================
score_grade(Score, 'A') :- Score >= 80, !.
score_grade(Score, 'B') :- Score >= 60, !.
score_grade(Score, 'C') :- Score >= 40, !.
score_grade(_, 'D').
