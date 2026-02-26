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

    % 4. Generate Advice (Collect all matching rules)
    findall(AdvicePart, health_advice(Profile, Stage, BMI, BMICategory, AdvicePart), AdviceList),
    
    % Remove duplicates if any rules overlapped
    sort(AdviceList, UniqueAdviceList),

    % Ensure there is at least one piece of advice! (Fallback)
    (   UniqueAdviceList == []
    ->  FinalAdviceList = ['Your profile looks incredibly balanced right now! Keep up your current routines, focusing on consistency and enjoying the process.']
    ;   FinalAdviceList = UniqueAdviceList
    ),

    % 5. Package everything up into a Dict
    Result = _{
        stage: Stage,
        focusArea: FocusArea,
        bmi: BMI,
        bmiCategory: BMICategory,
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
lifecycle_stage(_, 'Unknown Stage'). % Catch-all

%======================================================================
% 2. Focus Area Rules (Tied to Lifecycle)
%======================================================================
focus_area(_, 'Infant', 'Early Development, Motor Skills, & Neurological Growth') :- !.
focus_area(_, 'Child', 'Cognitive Development & Fundamental Nutrition') :- !.
focus_area(_, 'Teenager', 'Hormonal Balance, Mental Wellbeing, & Bone Density') :- !.
focus_area(_, 'Young Adult', 'Fitness Foundation, Metabolic Health, & Stress Resilience') :- !.
focus_area(_, 'Adult', 'Cardiovascular Maintenance, Joint Health, & Disease Prevention') :- !.
focus_area(_, 'Senior', 'Mobility Preservation, Cognitive Sharpness, & Longevity') :- !.

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
% These rules map the complex profile data to specific recommendations.
%======================================================================

% --- BMI Advice ---
health_advice(_, _, _, 'Underweight', 'Your BMI indicates you might be a bit underweight. Consider speaking with a nutritionist or dietitian—they can help you safely add delicious, nutrient-dense calories to your daily meals!').
health_advice(_, _, _, 'Overweight', 'Your BMI is currently in the overweight category. Don`t stress! Focusing on a slight caloric deficit and simply increasing your daily step count can work wonders for your long-term health.').
health_advice(_, _, _, 'Obese', 'Your BMI indicates obesity, which can put extra strain on your body. The best first step is to partner with a healthcare professional to create a sustainable, low-impact plan that you actually enjoy.').

% --- Sleep Advice ---
health_advice(Profile, _, _, _, 'We noticed you are getting less than 7 hours of sleep. Try prioritizing 7-8 hours a night—it is the ultimate cheat code for recovery, focus, and hormone balance!') :-
    Sleep = Profile.get(sleepHours),
    Sleep < 7.

% --- Activity Advice ---
health_advice(Profile, _, _, _, 'A sedentary lifestyle can sneak up on you! Try to gradually sneak in 30 minutes of light movement today, even if it is just a brisk walk while listening to a podcast.') :-
    Activity = Profile.get(activityLevel),
    Activity = 'sedentary'.

% --- Diet Evaluation Rules (Intersecting with Goals) ---
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

health_advice(Profile, _, _, _, 'Since heart health is a major goal for you, let`s watch that high-fat diet. Try shifting towards "healthy fats" like avocados, nuts, and olive oil to protect your cardiovascular system.') :-
    Goals = Profile.get(goals),
    member('heart_health', Goals),
    Diet = Profile.get(dietType),
    Diet = 'high_fat'.

% --- Symptom Pattern Matching Rules ---
health_advice(Profile, _, _, _, '**BURNOUT WARNING**: High stress, constant fatigue, and brain fog are your body`s emergency signals. Please prioritize active recovery today, take mental breaks, and don`t hesitate to seek professional support. You cannot pour from an empty cup!') :-
    Stress = Profile.get(stressLevel),
    Stress > 7,
    Symptoms = Profile.get(symptoms),
    member('fatigue', Symptoms),
    member('poor_concentration', Symptoms).

health_advice(Profile, _, _, _, 'Chronic back pain mixed with a lot of sitting usually points to a weak core. A super basic 5-minute daily core-strengthening routine could change your life!') :-
    Activity = Profile.get(activityLevel),
    Activity = 'sedentary',
    Symptoms = Profile.get(symptoms),
    member('back_pain', Symptoms).

health_advice(Profile, _, _, _, 'Frequent headaches can be awful! If you look at screens all day, remember the 20-20-20 rule to rest your eyes, and keep a water bottle glued to your side.') :-
    Symptoms = Profile.get(symptoms),
    member('headaches', Symptoms).

% --- Goal Personalization (Direct Responses) ---
health_advice(Profile, _, _, _, 'To hit your goal of reducing stress, try adding just 10 minutes of mindfulness or deep breathing to your morning. It genuinely lowers cortisol levels.') :-
    Goals = Profile.get(goals),
    member('reduce_stress', Goals).

health_advice(Profile, _, _, _, 'Improving sleep is a fantastic goal! Try establishing a strict "wind-down" routine: absolutely no screens 1 hour before bed, and keep your room cool and dark.') :-
    Goals = Profile.get(goals),
    member('improve_sleep', Goals).

% --- Stage-Specific Baseline Advice (Dynamic) ---

% Young Adult (Depends on Activity)
health_advice(Profile, 'Young Adult', _, _, 'As a Young Adult, you are in the prime window to build peak bone mass and heart capacity. Since you are already active, keep safely pushing your limits to build an unbreakable foundation for later years!') :-
    Activity = Profile.get(activityLevel),
    (Activity = 'moderate' ; Activity = 'active').

health_advice(Profile, 'Young Adult', _, _, 'You are in the prime window to build peak bone mass, but your low activity puts you at risk for future issues. Start some light weight-bearing exercises this week—your future self will thank you!') :-
    Activity = Profile.get(activityLevel),
    (Activity = 'sedentary' ; Activity = 'light').

% Adult (Depends on Stress)
health_advice(Profile, 'Adult', _, _, 'Metabolism naturally slows down. Since your stress is managed well, focus on nutrient-dense foods to maintain your healthy baseline.') :-
    Stress = Profile.get(stressLevel),
    Stress < 6.

health_advice(Profile, 'Adult', _, _, 'Metabolism naturally slows down, and your high stress levels will aggressively increase fat storage around your organs. Managing stress is your #1 priority right now.') :-
    Stress = Profile.get(stressLevel),
    Stress >= 6.

% Senior (Depends on Sleep)
health_advice(Profile, 'Senior', _, _, 'Focus on resistance training to combat sarcopenia (muscle loss). Your good sleep habits will perfectly support this muscle recovery.') :-
    Sleep = Profile.get(sleepHours),
    Sleep >= 7.

health_advice(Profile, 'Senior', _, _, 'To prevent cognitive decline and muscle loss, you MUST improve your sleep duration. Deep sleep is when the brain clears toxins.') :-
    Sleep = Profile.get(sleepHours),
    Sleep < 7.
